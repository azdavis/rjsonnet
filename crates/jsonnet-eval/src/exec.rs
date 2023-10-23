use crate::val::{Env, RecValKind, Subst, Val};
use jsonnet_expr::{Arenas, BinaryOp, Expr, ExprData, Id, Prim, Str, Visibility};
use rustc_hash::{FxHashMap, FxHashSet};
use std::cmp::Ordering;

const EPSILON: f64 = 0.0001;

#[derive(Debug)]
pub enum Error {
  Todo,
  ArrayIdxNotInteger,
  ArrayIdxOutOfRange,
  DuplicateArgument,
  DuplicateField,
  IncompatibleTypes,
  NoSuchArgument,
  NoSuchFieldName,
  TooManyArguments,
  CmpNan,
  CmpInf,
  User(Str),
  NoExpr,
  Function,
}

pub type Result<T = Val> = std::result::Result<T, Error>;

/// # Errors
///
/// If execution failed.
///
/// # Panics
///
/// If the expr wasn't checked.
pub fn exec(env: &Env, ars: &Arenas, expr: Expr) -> Result {
  // TODO implement a cache on expr to avoid re-computing lazy exprs? but we would also need to
  // consider the env in which the expr is executed
  let Some(expr) = expr else { return Err(Error::NoExpr) };
  match &ars.expr[expr] {
    ExprData::Prim(p) => Ok(Val::Prim(*p)),
    ExprData::Object { asserts, fields } => {
      let mut named_fields = FxHashMap::<Str, (Visibility, Expr)>::default();
      for &(key, hid, val) in fields {
        match exec(env, ars, key)? {
          Val::Prim(Prim::String(s)) => {
            if named_fields.insert(s, (hid, val)).is_some() {
              return Err(Error::DuplicateField);
            }
          }
          Val::Prim(Prim::Null) => {}
          Val::Prim(_) | Val::Rec { .. } => return Err(Error::IncompatibleTypes),
        }
      }
      let kind = RecValKind::Object { asserts: asserts.clone(), fields: named_fields };
      Ok(Val::Rec { env: env.clone(), kind })
    }
    ExprData::ObjectComp { name, body, id, ary } => {
      let (elem_env, elems) = match exec(env, ars, *ary)? {
        Val::Rec { env, kind: RecValKind::Array(xs) } => (env, xs),
        Val::Prim(_) | Val::Rec { .. } => return Err(Error::IncompatibleTypes),
      };
      let mut fields = FxHashMap::<Str, (Visibility, Expr)>::default();
      for elem in elems {
        let mut env = env.clone();
        env.insert(*id, Subst::Expr(elem_env.clone(), elem));
        match exec(&env, ars, *name)? {
          Val::Prim(Prim::String(s)) => {
            // TODO should we continue here?
            let Some(body) = *body else { continue };
            // we want to do `[e/x]body` here?
            let body = match ars.expr[body] {
              ExprData::Prim(_) => body,
              _ => return Err(Error::Todo),
            };
            if fields.insert(s, (Visibility::Default, Some(body))).is_some() {
              return Err(Error::DuplicateField);
            }
          }
          Val::Prim(Prim::Null) => {}
          Val::Prim(_) | Val::Rec { .. } => return Err(Error::IncompatibleTypes),
        }
      }
      let kind = RecValKind::Object { asserts: Vec::new(), fields };
      Ok(Val::Rec { env: env.clone(), kind })
    }
    ExprData::Array(elems) => {
      let kind = RecValKind::Array(elems.clone());
      Ok(Val::Rec { env: env.clone(), kind })
    }
    ExprData::Subscript { on, idx } => match exec(env, ars, *on)? {
      Val::Rec { env: mut obj_env, kind: RecValKind::Object { asserts, fields } } => {
        let name = match exec(env, ars, *idx)? {
          Val::Prim(Prim::String(x)) => x,
          Val::Prim(_) | Val::Rec { .. } => return Err(Error::IncompatibleTypes),
        };
        let Some(&(_, body)) = fields.get(&name) else { return Err(Error::NoSuchFieldName) };
        let kind = RecValKind::Object { asserts: asserts.clone(), fields: fields.clone() };
        let this = Val::Rec { env: obj_env.clone(), kind };
        obj_env.insert(Id::SELF, Subst::Val(this));
        obj_env.insert(Id::SUPER, Subst::Val(Val::empty_object()));
        for assert in asserts {
          exec(&obj_env, ars, assert)?;
        }
        exec(&obj_env, ars, body)
      }
      Val::Rec { env: ary_env, kind: RecValKind::Array(elems) } => {
        let idx = match exec(env, ars, *idx)? {
          Val::Prim(Prim::Number(x)) => x,
          Val::Prim(_) | Val::Rec { .. } => return Err(Error::IncompatibleTypes),
        };
        let idx_floor = idx.floor();
        let diff = idx - idx_floor;
        if diff.abs() > EPSILON {
          return Err(Error::ArrayIdxNotInteger);
        }
        if idx_floor < 0.0 || idx_floor > f64::from(u32::MAX) {
          return Err(Error::ArrayIdxOutOfRange);
        }
        #[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
        let idx = idx_floor as u32;
        let Ok(idx) = usize::try_from(idx) else { return Err(Error::ArrayIdxOutOfRange) };
        match elems.get(idx) {
          Some(&elem) => exec(&ary_env, ars, elem),
          None => Err(Error::ArrayIdxOutOfRange),
        }
      }
      Val::Rec { .. } | Val::Prim(_) => Err(Error::IncompatibleTypes),
    },
    ExprData::Call { func, positional, named } => {
      let (func_env, mut params, body) = match exec(env, ars, *func)? {
        Val::Rec { env, kind: RecValKind::Function { params, body } } => (env, params, body),
        Val::Rec { .. } | Val::Prim(_) => return Err(Error::IncompatibleTypes),
      };
      if positional.len() + named.len() > params.len() {
        return Err(Error::TooManyArguments);
      }
      let mut provided = FxHashSet::<Id>::default();
      for ((id, param), &arg) in params.iter_mut().zip(positional) {
        *param = arg;
        assert!(provided.insert(*id), "duplicate function param should be forbidden by check");
      }
      for &(arg_name, arg) in named {
        if !provided.insert(arg_name) {
          return Err(Error::DuplicateArgument);
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
          return Err(Error::NoSuchArgument);
        }
      }
      exec_local(&func_env, &params, ars, body)
    }
    ExprData::Id(id) => match env.get(*id) {
      Subst::Val(v) => Ok(v.clone()),
      Subst::Expr(env, expr) => exec(env, ars, *expr),
    },
    ExprData::Local { binds, body } => exec_local(env, binds, ars, *body),
    ExprData::If { cond, yes, no } => {
      let b = match exec(env, ars, *cond)? {
        Val::Prim(Prim::Bool(x)) => x,
        Val::Prim(_) | Val::Rec { .. } => return Err(Error::IncompatibleTypes),
      };
      let &expr = if b { yes } else { no };
      exec(env, ars, expr)
    }
    ExprData::BinaryOp { lhs, op, rhs } => match op {
      // add
      BinaryOp::Add => match (exec(env, ars, *lhs)?, exec(env, ars, *rhs)?) {
        (Val::Prim(Prim::String(lhs)), rhs) => {
          let rhs = str_conv(rhs);
          Ok(Val::Prim(Prim::String(str_concat(lhs, rhs))))
        }
        (lhs, Val::Prim(Prim::String(rhs))) => {
          let lhs = str_conv(lhs);
          Ok(Val::Prim(Prim::String(str_concat(lhs, rhs))))
        }
        (Val::Prim(Prim::Number(lhs)), Val::Prim(Prim::Number(rhs))) => {
          Ok(Val::Prim(Prim::Number(lhs + rhs)))
        }
        _ => Err(Error::Todo),
      },
      // arithmetic
      BinaryOp::Mul => float_op(env, ars, *lhs, *rhs, std::ops::Mul::mul),
      BinaryOp::Div => float_op(env, ars, *lhs, *rhs, std::ops::Div::div),
      BinaryOp::Sub => float_op(env, ars, *lhs, *rhs, std::ops::Sub::sub),
      // bitwise
      BinaryOp::Shl => int_op(env, ars, *lhs, *rhs, std::ops::Shl::shl),
      BinaryOp::Shr => int_op(env, ars, *lhs, *rhs, std::ops::Shr::shr),
      BinaryOp::BitAnd => int_op(env, ars, *lhs, *rhs, std::ops::BitAnd::bitand),
      BinaryOp::BitXor => int_op(env, ars, *lhs, *rhs, std::ops::BitXor::bitxor),
      BinaryOp::BitOr => int_op(env, ars, *lhs, *rhs, std::ops::BitOr::bitor),
      // comparison
      BinaryOp::Lt => cmp_op(env, ars, *lhs, *rhs, Ordering::is_lt),
      BinaryOp::LtEq => cmp_op(env, ars, *lhs, *rhs, Ordering::is_le),
      BinaryOp::Gt => cmp_op(env, ars, *lhs, *rhs, Ordering::is_gt),
      BinaryOp::GtEq => cmp_op(env, ars, *lhs, *rhs, Ordering::is_ge),
      // logical
      BinaryOp::LogicalAnd => match exec(env, ars, *lhs)? {
        Val::Prim(Prim::Bool(true)) => exec(env, ars, *rhs),
        Val::Prim(Prim::Bool(false)) => Ok(Val::Prim(Prim::Bool(false))),
        Val::Prim(_) | Val::Rec { .. } => Err(Error::IncompatibleTypes),
      },
      BinaryOp::LogicalOr => match exec(env, ars, *lhs)? {
        Val::Prim(Prim::Bool(true)) => Ok(Val::Prim(Prim::Bool(true))),
        Val::Prim(Prim::Bool(false)) => exec(env, ars, *rhs),
        Val::Prim(_) | Val::Rec { .. } => Err(Error::IncompatibleTypes),
      },
    },
    ExprData::UnaryOp { .. } => Err(Error::Todo),
    ExprData::Function { params, body } => {
      let kind = RecValKind::Function { params: params.clone(), body: *body };
      Ok(Val::Rec { env: env.clone(), kind })
    }
    ExprData::Error(inner) => {
      let val = exec(env, ars, *inner)?;
      Err(Error::User(str_conv(val)))
    }
  }
}

fn float_pair(env: &Env, ars: &Arenas, a: Expr, b: Expr) -> Result<[f64; 2]> {
  match (exec(env, ars, a)?, exec(env, ars, b)?) {
    (Val::Prim(Prim::Number(a)), Val::Prim(Prim::Number(b))) => Ok([a, b]),
    _ => Err(Error::IncompatibleTypes),
  }
}

fn float_op<F>(env: &Env, ars: &Arenas, lhs: Expr, rhs: Expr, f: F) -> Result
where
  F: FnOnce(f64, f64) -> f64,
{
  let [lhs, rhs] = float_pair(env, ars, lhs, rhs)?;
  Ok(Val::Prim(Prim::Number(f(lhs, rhs))))
}

fn int_op<F>(env: &Env, ars: &Arenas, lhs: Expr, rhs: Expr, f: F) -> Result
where
  F: FnOnce(i64, i64) -> i64,
{
  let ns = float_pair(env, ars, lhs, rhs)?;
  #[allow(clippy::cast_possible_truncation)]
  let [lhs, rhs] = ns.map(|x| x as i64);
  #[allow(clippy::cast_precision_loss)]
  let n = f(lhs, rhs) as f64;
  Ok(Val::Prim(Prim::Number(n)))
}

fn cmp_op<F>(env: &Env, ars: &Arenas, lhs: Expr, rhs: Expr, f: F) -> Result
where
  F: FnOnce(Ordering) -> bool,
{
  let lhs = exec(env, ars, lhs)?;
  let rhs = exec(env, ars, rhs)?;
  let ord = cmp_val(ars, &lhs, &rhs)?;
  Ok(Val::Prim(Prim::Bool(f(ord))))
}

fn cmp_val(ars: &Arenas, lhs: &Val, rhs: &Val) -> Result<Ordering> {
  match (lhs, rhs) {
    (Val::Prim(lhs), Val::Prim(rhs)) => match (lhs, rhs) {
      (Prim::String(lhs), Prim::String(rhs)) => Ok(ars.str.get(*lhs).cmp(ars.str.get(*rhs))),
      (Prim::Number(lhs), Prim::Number(rhs)) => {
        if lhs.is_nan() || rhs.is_nan() {
          Err(Error::CmpNan)
        } else if rhs.is_infinite() || lhs.is_infinite() {
          Err(Error::CmpInf)
        } else {
          Ok(lhs.partial_cmp(rhs).expect("not nan or inf"))
        }
      }
      _ => Err(Error::IncompatibleTypes),
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
              let lhs = exec(le, ars, lhs)?;
              let rhs = exec(re, ars, rhs)?;
              match cmp_val(ars, &lhs, &rhs)? {
                Ordering::Equal => {}
                ord => break ord,
              }
            }
          }
        };
        Ok(ord)
      }
      _ => Err(Error::IncompatibleTypes),
    },
    _ => Err(Error::IncompatibleTypes),
  }
}

fn exec_local(_: &Env, _: &[(Id, Expr)], _: &Arenas, _: Expr) -> Result {
  Err(Error::Todo)
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
