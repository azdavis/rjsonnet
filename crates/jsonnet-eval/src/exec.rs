//! Executing Jsonnet expression to produce Jsonnet values.

use crate::error::{self, Result};
use crate::val::jsonnet::{Env, RecValKind, Std, Subst, Val};
use jsonnet_expr::{Arenas, BinaryOp, Expr, ExprData, ExprMust, Id, Number, Prim, Str, Visibility};
use rustc_hash::{FxHashMap, FxHashSet};
use std::cmp::Ordering;

const EPSILON: f64 = 0.0001;

/// Executes the Jsonnet expression to produce a Jsonnet value.
///
/// # Errors
///
/// If execution failed.
///
/// # Panics
///
/// If the expr wasn't checked.
pub fn get(env: &Env, ars: &Arenas, expr: Expr) -> Result<Val> {
  // TODO implement a cache on expr to avoid re-computing lazy exprs? but we would also need to
  // consider the env in which the expr is executed
  let expr = expr.expect("no expr");
  let mk_error = |kind: error::Kind| Err(error::Error::Exec { expr, kind });
  match &ars.expr[expr] {
    ExprData::Prim(p) => Ok(Val::Prim(p.clone())),
    ExprData::Object { asserts, fields } => {
      let mut named_fields = FxHashMap::<Str, (Visibility, Expr)>::default();
      for &(key, hid, val) in fields {
        match get(env, ars, key)? {
          Val::Prim(Prim::String(s)) => {
            if named_fields.insert(s, (hid, val)).is_some() {
              return mk_error(error::Kind::DuplicateField);
            }
          }
          Val::Prim(Prim::Null) => {}
          _ => return mk_error(error::Kind::IncompatibleTypes),
        }
      }
      let kind = RecValKind::Object { asserts: asserts.clone(), fields: named_fields };
      Ok(Val::Rec { env: env.clone(), kind })
    }
    ExprData::ObjectComp { name, body, id, ary } => {
      let Val::Rec { env: elem_env, kind: RecValKind::Array(elems) } = get(env, ars, *ary)? else {
        return mk_error(error::Kind::IncompatibleTypes);
      };
      let mut fields = FxHashMap::<Str, (Visibility, Expr)>::default();
      for elem in elems {
        let mut env = env.clone();
        env.insert(*id, Subst::Expr(elem_env.clone(), elem));
        match get(&env, ars, *name)? {
          Val::Prim(Prim::String(s)) => {
            // TODO should we continue here?
            let Some(body) = *body else { continue };
            // we want to do `[e/x]body` here?
            let body = match ars.expr[body] {
              ExprData::Prim(_) => body,
              _ => return mk_error(error::Kind::Todo("subst for object comp")),
            };
            if fields.insert(s, (Visibility::Default, Some(body))).is_some() {
              return mk_error(error::Kind::DuplicateField);
            }
          }
          Val::Prim(Prim::Null) => {}
          _ => return mk_error(error::Kind::IncompatibleTypes),
        }
      }
      let kind = RecValKind::Object { asserts: Vec::new(), fields };
      Ok(Val::Rec { env: env.clone(), kind })
    }
    ExprData::Array(elems) => {
      let kind = RecValKind::Array(elems.clone());
      Ok(Val::Rec { env: env.clone(), kind })
    }
    ExprData::Subscript { on, idx } => match get(env, ars, *on)? {
      Val::Rec { env: mut obj_env, kind: RecValKind::Object { asserts, fields } } => {
        let Val::Prim(Prim::String(name)) = get(env, ars, *idx)? else {
          return mk_error(error::Kind::IncompatibleTypes);
        };
        let Some(&(_, body)) = fields.get(&name) else {
          return mk_error(error::Kind::NoSuchFieldName);
        };
        let kind = RecValKind::Object { asserts: asserts.clone(), fields: fields.clone() };
        let this = Val::Rec { env: obj_env.clone(), kind };
        obj_env.insert(Id::SELF, Subst::Val(this));
        obj_env.insert(Id::SUPER, Subst::Val(Val::empty_object()));
        for assert in asserts {
          get(&obj_env, ars, assert)?;
        }
        get(&obj_env, ars, body)
      }
      Val::Rec { env: ary_env, kind: RecValKind::Array(elems) } => {
        let Val::Prim(Prim::Number(idx)) = get(env, ars, *idx)? else {
          return mk_error(error::Kind::IncompatibleTypes);
        };
        let idx = idx.value();
        let idx_floor = idx.floor();
        let diff = idx - idx_floor;
        if diff.abs() > EPSILON {
          return mk_error(error::Kind::ArrayIdxNotInteger);
        }
        if idx_floor < 0.0 || idx_floor > f64::from(u32::MAX) {
          return mk_error(error::Kind::ArrayIdxOutOfRange);
        }
        #[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
        let idx = idx_floor as u32;
        let Ok(idx) = usize::try_from(idx) else {
          return mk_error(error::Kind::ArrayIdxOutOfRange);
        };
        match elems.get(idx) {
          Some(&elem) => get(&ary_env, ars, elem),
          None => mk_error(error::Kind::ArrayIdxOutOfRange),
        }
      }
      Val::Std(_) | Val::Rec { .. } | Val::Prim(_) => mk_error(error::Kind::IncompatibleTypes),
    },
    ExprData::Call { func, positional, named } => match get(env, ars, *func)? {
      Val::Rec { env: func_env, kind: RecValKind::Function { mut params, body } } => {
        if positional.len() + named.len() > params.len() {
          return mk_error(error::Kind::TooManyArguments);
        }
        let mut provided = FxHashSet::<Id>::default();
        for ((id, param), &arg) in params.iter_mut().zip(positional) {
          *param = arg;
          assert!(provided.insert(*id), "duplicate function param should be forbidden by check");
        }
        for &(arg_name, arg) in named {
          if !provided.insert(arg_name) {
            return mk_error(error::Kind::DuplicateArgument);
          }
          // we're getting a little fancy here. this iterates across the mutable params, and if we
          // could find a param whose name matches the arg's name, then this sets the param to that
          // arg and short circuits with true. note `==` with comparing the names and `=` with setting
          // the actual exprs. note the usage of `bool::then` with `find_map` and `is_none`.
          let failed_to_set_arg = params
            .iter_mut()
            .find_map(|(param_name, param)| (*param_name == arg_name).then(|| *param = arg))
            .is_none();
          if failed_to_set_arg {
            return mk_error(error::Kind::NoSuchArgument);
          }
        }
        exec_local(&func_env, &params, ars, body)
      }
      Val::Std(std_val) => match std_val {
        Std::Cmp => {
          if !named.is_empty() {
            return mk_error(error::Kind::StdFuncNamedArgs);
          }
          let [lhs, rhs] = positional[..] else {
            return mk_error(error::Kind::StdFuncWrongNumArgs(2, positional.len()));
          };
          cmp_op(expr, env, ars, lhs, rhs, |ord| {
            let num = match ord {
              Ordering::Less => Number::negative_one(),
              Ordering::Equal => Number::positive_zero(),
              Ordering::Greater => Number::positive_one(),
            };
            Prim::Number(num)
          })
        }
        Std::Equals => {
          if !named.is_empty() {
            return mk_error(error::Kind::StdFuncNamedArgs);
          }
          let [lhs, rhs] = positional[..] else {
            return mk_error(error::Kind::StdFuncWrongNumArgs(2, positional.len()));
          };
          cmp_bool_op(expr, env, ars, lhs, rhs, Ordering::is_eq)
        }
      },
      Val::Rec { .. } | Val::Prim(_) => mk_error(error::Kind::IncompatibleTypes),
    },
    ExprData::Id(id) => match env.get(*id) {
      Subst::Val(v) => Ok(v.clone()),
      Subst::Expr(env, expr) => get(env, ars, *expr),
    },
    ExprData::Local { binds, body } => exec_local(env, binds, ars, *body),
    ExprData::If { cond, yes, no } => {
      let Val::Prim(Prim::Bool(b)) = get(env, ars, *cond)? else {
        return mk_error(error::Kind::IncompatibleTypes);
      };
      let &expr = if b { yes } else { no };
      get(env, ars, expr)
    }
    ExprData::BinaryOp { lhs, op, rhs } => match op {
      // add
      BinaryOp::Add => match (get(env, ars, *lhs)?, get(env, ars, *rhs)?) {
        (Val::Prim(Prim::String(lhs)), rhs) => {
          let rhs = str_conv(rhs);
          Ok(Val::Prim(Prim::String(str_concat(lhs, rhs))))
        }
        (lhs, Val::Prim(Prim::String(rhs))) => {
          let lhs = str_conv(lhs);
          Ok(Val::Prim(Prim::String(str_concat(lhs, rhs))))
        }
        (Val::Prim(Prim::Number(lhs)), Val::Prim(Prim::Number(rhs))) => {
          let n = match Number::try_from(lhs.value() + rhs.value()) {
            Ok(n) => n,
            Err(inf) => return mk_error(error::Kind::Infinite(inf)),
          };
          Ok(Val::Prim(Prim::Number(n)))
        }
        _ => mk_error(error::Kind::Todo("+ for non-prim")),
      },
      // arithmetic
      BinaryOp::Mul => float_op(expr, env, ars, *lhs, *rhs, std::ops::Mul::mul),
      BinaryOp::Div => float_op(expr, env, ars, *lhs, *rhs, std::ops::Div::div),
      BinaryOp::Sub => float_op(expr, env, ars, *lhs, *rhs, std::ops::Sub::sub),
      // bitwise
      BinaryOp::Shl => int_op(expr, env, ars, *lhs, *rhs, std::ops::Shl::shl),
      BinaryOp::Shr => int_op(expr, env, ars, *lhs, *rhs, std::ops::Shr::shr),
      BinaryOp::BitAnd => int_op(expr, env, ars, *lhs, *rhs, std::ops::BitAnd::bitand),
      BinaryOp::BitXor => int_op(expr, env, ars, *lhs, *rhs, std::ops::BitXor::bitxor),
      BinaryOp::BitOr => int_op(expr, env, ars, *lhs, *rhs, std::ops::BitOr::bitor),
      // comparison
      BinaryOp::Lt => cmp_bool_op(expr, env, ars, *lhs, *rhs, Ordering::is_lt),
      BinaryOp::LtEq => cmp_bool_op(expr, env, ars, *lhs, *rhs, Ordering::is_le),
      BinaryOp::Gt => cmp_bool_op(expr, env, ars, *lhs, *rhs, Ordering::is_gt),
      BinaryOp::GtEq => cmp_bool_op(expr, env, ars, *lhs, *rhs, Ordering::is_ge),
      // logical
      BinaryOp::LogicalAnd => {
        let Val::Prim(Prim::Bool(b)) = get(env, ars, *lhs)? else {
          return mk_error(error::Kind::IncompatibleTypes);
        };
        if b {
          get(env, ars, *rhs)
        } else {
          Ok(Val::Prim(Prim::Bool(false)))
        }
      }
      BinaryOp::LogicalOr => {
        let Val::Prim(Prim::Bool(b)) = get(env, ars, *lhs)? else {
          return mk_error(error::Kind::IncompatibleTypes);
        };
        if b {
          Ok(Val::Prim(Prim::Bool(true)))
        } else {
          get(env, ars, *rhs)
        }
      }
    },
    ExprData::UnaryOp { .. } => mk_error(error::Kind::Todo("unary ops")),
    ExprData::Function { params, body } => {
      let kind = RecValKind::Function { params: params.clone(), body: *body };
      Ok(Val::Rec { env: env.clone(), kind })
    }
    ExprData::Error(inner) => {
      let val = get(env, ars, *inner)?;
      mk_error(error::Kind::User(str_conv(val)))
    }
    ExprData::Import { .. } => todo!(),
  }
}

fn number_pair(expr: ExprMust, env: &Env, ars: &Arenas, a: Expr, b: Expr) -> Result<[Number; 2]> {
  match (get(env, ars, a)?, get(env, ars, b)?) {
    (Val::Prim(Prim::Number(a)), Val::Prim(Prim::Number(b))) => Ok([a, b]),
    _ => Err(error::Error::Exec { expr, kind: error::Kind::IncompatibleTypes }),
  }
}

fn float_op<F>(expr: ExprMust, env: &Env, ars: &Arenas, lhs: Expr, rhs: Expr, f: F) -> Result<Val>
where
  F: FnOnce(f64, f64) -> f64,
{
  let [lhs, rhs] = number_pair(expr, env, ars, lhs, rhs)?;
  let n = match Number::try_from(f(lhs.value(), rhs.value())) {
    Ok(n) => n,
    Err(inf) => return Err(error::Error::Exec { expr, kind: error::Kind::Infinite(inf) }),
  };
  Ok(Val::Prim(Prim::Number(n)))
}

fn int_op<F>(expr: ExprMust, env: &Env, ars: &Arenas, lhs: Expr, rhs: Expr, f: F) -> Result<Val>
where
  F: FnOnce(i64, i64) -> i64,
{
  let ns = number_pair(expr, env, ars, lhs, rhs)?;
  #[allow(clippy::cast_possible_truncation)]
  let [lhs, rhs] = ns.map(|x| x.value() as i64);
  #[allow(clippy::cast_precision_loss)]
  let n = f(lhs, rhs) as f64;
  let n = match Number::try_from(n) {
    Ok(n) => n,
    Err(inf) => return Err(error::Error::Exec { expr, kind: error::Kind::Infinite(inf) }),
  };
  Ok(Val::Prim(Prim::Number(n)))
}

fn cmp_op<F>(expr: ExprMust, env: &Env, ars: &Arenas, lhs: Expr, rhs: Expr, f: F) -> Result<Val>
where
  F: FnOnce(Ordering) -> Prim,
{
  let lhs = get(env, ars, lhs)?;
  let rhs = get(env, ars, rhs)?;
  let ord = cmp_val(expr, ars, &lhs, &rhs)?;
  Ok(Val::Prim(f(ord)))
}

fn cmp_bool_op<F>(
  expr: ExprMust,
  env: &Env,
  ars: &Arenas,
  lhs: Expr,
  rhs: Expr,
  f: F,
) -> Result<Val>
where
  F: FnOnce(Ordering) -> bool,
{
  cmp_op(expr, env, ars, lhs, rhs, |x| Prim::Bool(f(x)))
}

fn cmp_val(expr: ExprMust, ars: &Arenas, lhs: &Val, rhs: &Val) -> Result<Ordering> {
  match (lhs, rhs) {
    (Val::Prim(lhs), Val::Prim(rhs)) => match (lhs, rhs) {
      (Prim::String(lhs), Prim::String(rhs)) => Ok(ars.str.get(lhs).cmp(ars.str.get(rhs))),
      (Prim::Number(lhs), Prim::Number(rhs)) => Ok(lhs.cmp(rhs)),
      _ => Err(error::Error::Exec { expr, kind: error::Kind::IncompatibleTypes }),
    },
    (Val::Rec { env: le, kind: lhs }, Val::Rec { env: re, kind: rhs }) => match (lhs, rhs) {
      (RecValKind::Array(lhs), RecValKind::Array(rhs)) => {
        let mut lhs = lhs.iter();
        let mut rhs = rhs.iter();
        let ord = loop {
          match (lhs.next(), rhs.next()) {
            (None, Some(_)) => break Ordering::Less,
            (None, None) => break Ordering::Equal,
            (Some(_), None) => break Ordering::Greater,
            (Some(&lhs), Some(&rhs)) => {
              let lhs = get(le, ars, lhs)?;
              let rhs = get(re, ars, rhs)?;
              match cmp_val(expr, ars, &lhs, &rhs)? {
                Ordering::Equal => {}
                ord => break ord,
              }
            }
          }
        };
        Ok(ord)
      }
      _ => Err(error::Error::Exec { expr, kind: error::Kind::IncompatibleTypes }),
    },
    _ => Err(error::Error::Exec { expr, kind: error::Kind::IncompatibleTypes }),
  }
}

fn exec_local(prev_env: &Env, binds: &[(Id, Expr)], ars: &Arenas, body: Expr) -> Result<Val> {
  let mut env = prev_env.clone();
  for &(id, expr) in binds {
    env.insert(id, Subst::Expr(prev_env.clone(), expr));
  }
  get(&env, ars, body)
}

#[allow(clippy::needless_pass_by_value)]
fn str_conv(val: Val) -> Str {
  match val {
    Val::Prim(Prim::String(s)) => s,
    _ => Str::TODO,
  }
}

fn str_concat(_: Str, _: Str) -> Str {
  Str::TODO
}
