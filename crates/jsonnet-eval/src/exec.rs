//! Executing Jsonnet expression to produce Jsonnet values.

use crate::val::{Env, RecValKind, Subst, Val};
use jsonnet_expr::{Arenas, BinaryOp, Expr, ExprData, Id, Infinite, Number, Prim, Str, Visibility};
use rustc_hash::{FxHashMap, FxHashSet};
use std::cmp::Ordering;

const EPSILON: f64 = 0.0001;

#[derive(Debug)]
pub enum Error {
  Todo(&'static str),
  ArrayIdxNotInteger,
  ArrayIdxOutOfRange,
  DuplicateArgument,
  DuplicateField,
  IncompatibleTypes,
  NoSuchArgument,
  NoSuchFieldName,
  TooManyArguments,
  Infinite(Infinite),
  User(Str),
  /// not an actual error from `error`
  NoExpr,
}

impl From<Infinite> for Error {
  fn from(value: Infinite) -> Self {
    Self::Infinite(value)
  }
}

pub type Result<T = Val, E = Error> = std::result::Result<T, E>;

/// Executes the Jsonnet expression to produce a Jsonnet value.
///
/// # Errors
///
/// If execution failed.
///
/// # Panics
///
/// If the expr wasn't checked.
pub fn get(env: &Env, ars: &Arenas, expr: Expr) -> Result {
  // TODO implement a cache on expr to avoid re-computing lazy exprs? but we would also need to
  // consider the env in which the expr is executed
  let Some(expr) = expr else { return Err(Error::NoExpr) };
  match &ars.expr[expr] {
    ExprData::Prim(p) => Ok(Val::Prim(*p)),
    ExprData::Object { asserts, fields } => {
      let mut named_fields = FxHashMap::<Str, (Visibility, Expr)>::default();
      for &(key, hid, val) in fields {
        match get(env, ars, key)? {
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
      let (elem_env, elems) = match get(env, ars, *ary)? {
        Val::Rec { env, kind: RecValKind::Array(xs) } => (env, xs),
        Val::Prim(_) | Val::Rec { .. } => return Err(Error::IncompatibleTypes),
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
              _ => return Err(Error::Todo("subst for object comp")),
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
    ExprData::Subscript { on, idx } => match get(env, ars, *on)? {
      Val::Rec { env: mut obj_env, kind: RecValKind::Object { asserts, fields } } => {
        let name = match get(env, ars, *idx)? {
          Val::Prim(Prim::String(x)) => x,
          Val::Prim(_) | Val::Rec { .. } => return Err(Error::IncompatibleTypes),
        };
        let Some(&(_, body)) = fields.get(&name) else { return Err(Error::NoSuchFieldName) };
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
        let idx = match get(env, ars, *idx)? {
          Val::Prim(Prim::Number(x)) => x.value(),
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
          Some(&elem) => get(&ary_env, ars, elem),
          None => Err(Error::ArrayIdxOutOfRange),
        }
      }
      Val::Rec { .. } | Val::Prim(_) => Err(Error::IncompatibleTypes),
    },
    ExprData::Call { func, positional, named } => {
      let (func_env, mut params, body) = match get(env, ars, *func)? {
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
      Subst::Expr(env, expr) => get(env, ars, *expr),
    },
    ExprData::Local { binds, body } => exec_local(env, binds, ars, *body),
    ExprData::If { cond, yes, no } => {
      let b = match get(env, ars, *cond)? {
        Val::Prim(Prim::Bool(x)) => x,
        Val::Prim(_) | Val::Rec { .. } => return Err(Error::IncompatibleTypes),
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
          let n = Number::try_from(lhs.value() + rhs.value())?;
          Ok(Val::Prim(Prim::Number(n)))
        }
        _ => Err(Error::Todo("+ for non-prim")),
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
      BinaryOp::LogicalAnd => match get(env, ars, *lhs)? {
        Val::Prim(Prim::Bool(true)) => get(env, ars, *rhs),
        Val::Prim(Prim::Bool(false)) => Ok(Val::Prim(Prim::Bool(false))),
        Val::Prim(_) | Val::Rec { .. } => Err(Error::IncompatibleTypes),
      },
      BinaryOp::LogicalOr => match get(env, ars, *lhs)? {
        Val::Prim(Prim::Bool(true)) => Ok(Val::Prim(Prim::Bool(true))),
        Val::Prim(Prim::Bool(false)) => get(env, ars, *rhs),
        Val::Prim(_) | Val::Rec { .. } => Err(Error::IncompatibleTypes),
      },
    },
    ExprData::UnaryOp { .. } => Err(Error::Todo("unary ops")),
    ExprData::Function { params, body } => {
      let kind = RecValKind::Function { params: params.clone(), body: *body };
      Ok(Val::Rec { env: env.clone(), kind })
    }
    ExprData::Error(inner) => {
      let val = get(env, ars, *inner)?;
      Err(Error::User(str_conv(val)))
    }
  }
}

fn number_pair(env: &Env, ars: &Arenas, a: Expr, b: Expr) -> Result<[Number; 2]> {
  match (get(env, ars, a)?, get(env, ars, b)?) {
    (Val::Prim(Prim::Number(a)), Val::Prim(Prim::Number(b))) => Ok([a, b]),
    _ => Err(Error::IncompatibleTypes),
  }
}

fn float_op<F>(env: &Env, ars: &Arenas, lhs: Expr, rhs: Expr, f: F) -> Result
where
  F: FnOnce(f64, f64) -> f64,
{
  let [lhs, rhs] = number_pair(env, ars, lhs, rhs)?;
  let n = Number::try_from(f(lhs.value(), rhs.value()))?;
  Ok(Val::Prim(Prim::Number(n)))
}

fn int_op<F>(env: &Env, ars: &Arenas, lhs: Expr, rhs: Expr, f: F) -> Result
where
  F: FnOnce(i64, i64) -> i64,
{
  let ns = number_pair(env, ars, lhs, rhs)?;
  #[allow(clippy::cast_possible_truncation)]
  let [lhs, rhs] = ns.map(|x| x.value() as i64);
  #[allow(clippy::cast_precision_loss)]
  let n = f(lhs, rhs) as f64;
  let n = Number::try_from(n)?;
  Ok(Val::Prim(Prim::Number(n)))
}

fn cmp_op<F>(env: &Env, ars: &Arenas, lhs: Expr, rhs: Expr, f: F) -> Result
where
  F: FnOnce(Ordering) -> bool,
{
  let lhs = get(env, ars, lhs)?;
  let rhs = get(env, ars, rhs)?;
  let ord = cmp_val(ars, &lhs, &rhs)?;
  Ok(Val::Prim(Prim::Bool(f(ord))))
}

fn cmp_val(ars: &Arenas, lhs: &Val, rhs: &Val) -> Result<Ordering> {
  match (lhs, rhs) {
    (Val::Prim(lhs), Val::Prim(rhs)) => match (lhs, rhs) {
      (Prim::String(lhs), Prim::String(rhs)) => Ok(ars.str.get(*lhs).cmp(ars.str.get(*rhs))),
      (Prim::Number(lhs), Prim::Number(rhs)) => Ok(lhs.cmp(rhs)),
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
              let lhs = get(le, ars, lhs)?;
              let rhs = get(re, ars, rhs)?;
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
  Err(Error::Todo("locals"))
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
