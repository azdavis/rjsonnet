//! A lazy evaluator for jsonnet.
//!
//! From the [spec](https://jsonnet.org/ref/spec.html).

#![deny(clippy::pedantic, missing_debug_implementations, rust_2018_idioms)]
#![allow(clippy::too_many_lines)]

use jsonnet_hir::{Arenas, BinaryOp, Expr, ExprData, Id, Prim, Str, Visibility};
use rustc_hash::{FxHashMap, FxHashSet};
use std::cmp::Ordering;

#[derive(Debug, Default, Clone)]
pub struct Env {
  store: FxHashMap<Id, Subst>,
}

impl Env {
  fn insert(&mut self, id: Id, subst: Subst) {
    self.store.insert(id, subst);
  }

  fn get(&self, id: Id) -> &Subst {
    &self.store[&id]
  }
}

#[derive(Debug, Clone)]
enum Subst {
  Val(Val),
  Expr(Env, Expr),
}

/// The spec uses eager substitution but I suspect this is prohibitively non-performant. So we
/// separate values into primitives and recursive values. Recursive values contain expressions,
/// because Jsonnet itself has lazy semantics.
///
/// Because of this, and also because we choose to implement substitution lazily (as opposed to the
/// spec which expresses the semantics with eager substitution), we must therefore also carry with
/// recursive values an environment in which to do lazy substitutions.
///
/// Note that implementing substitution lazily is not meant to break with the spec. The execution
/// should be semantically equivalent.
///
/// We also consider errors values.
#[derive(Debug, Clone)]
pub enum Val {
  Prim(Prim),
  Rec { env: Env, kind: RecValKind },
}

impl Val {
  fn empty_object() -> Self {
    Self::Rec {
      env: Env::default(),
      kind: RecValKind::Object { asserts: Vec::new(), fields: FxHashMap::default() },
    }
  }
}

#[derive(Debug, Clone)]
pub enum RecValKind {
  Object {
    asserts: Vec<Expr>,
    fields: FxHashMap<Str, (Visibility, Expr)>,
  },
  Function {
    /// we'd like to get good performance for lookup by both index for positional arguments and name
    /// for keyword arguments, but to do that we'd need to something like double the memory and
    /// store both a vec and a map. which we could do but we choose to not right now.
    params: Vec<(Id, Expr)>,
    body: Expr,
  },
  Array(Vec<Expr>),
}

#[derive(Debug)]
pub enum EvalError {
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
}

type Eval<T = Val> = Result<T, EvalError>;

const EPSILON: f64 = 0.0001;

/// # Errors
///
/// If evaluation failed.
///
/// # Panics
///
/// If the expr wasn't checked.
pub fn eval(env: &Env, ars: &Arenas, expr: Expr) -> Eval {
  // TODO implement a cache on expr to avoid re-computing lazy exprs? but we would also need to
  // consider the env in which the expr is evaluated
  let Some(expr) = expr else { return Err(EvalError::NoExpr) };
  match &ars.expr[expr] {
    ExprData::Prim(p) => Ok(Val::Prim(*p)),
    ExprData::Object { asserts, fields } => {
      let mut named_fields = FxHashMap::<Str, (Visibility, Expr)>::default();
      for &(key, hid, val) in fields {
        match eval(env, ars, key)? {
          Val::Prim(Prim::String(s)) => {
            if named_fields.insert(s, (hid, val)).is_some() {
              return Err(EvalError::DuplicateField);
            }
          }
          Val::Prim(Prim::Null) => {}
          Val::Prim(_) | Val::Rec { .. } => return Err(EvalError::IncompatibleTypes),
        }
      }
      let kind = RecValKind::Object { asserts: asserts.clone(), fields: named_fields };
      Ok(Val::Rec { env: env.clone(), kind })
    }
    ExprData::ObjectComp { name, body, id, ary } => {
      let (elem_env, elems) = match eval(env, ars, *ary)? {
        Val::Rec { env, kind: RecValKind::Array(xs) } => (env, xs),
        Val::Prim(_) | Val::Rec { .. } => return Err(EvalError::IncompatibleTypes),
      };
      let mut fields = FxHashMap::<Str, (Visibility, Expr)>::default();
      for elem in elems {
        let mut env = env.clone();
        env.insert(*id, Subst::Expr(elem_env.clone(), elem));
        match eval(&env, ars, *name)? {
          Val::Prim(Prim::String(s)) => {
            // TODO should we continue here?
            let Some(body) = *body else { continue };
            // we want to do `[e/x]body` here?
            let body = match ars.expr[body] {
              ExprData::Prim(_) => body,
              _ => return Err(EvalError::Todo),
            };
            if fields.insert(s, (Visibility::Default, Some(body))).is_some() {
              return Err(EvalError::DuplicateField);
            }
          }
          Val::Prim(Prim::Null) => {}
          Val::Prim(_) | Val::Rec { .. } => return Err(EvalError::IncompatibleTypes),
        }
      }
      let kind = RecValKind::Object { asserts: Vec::new(), fields };
      Ok(Val::Rec { env: env.clone(), kind })
    }
    ExprData::Array(elems) => {
      let kind = RecValKind::Array(elems.clone());
      Ok(Val::Rec { env: env.clone(), kind })
    }
    ExprData::Subscript { on, idx } => match eval(env, ars, *on)? {
      Val::Rec { env: mut obj_env, kind: RecValKind::Object { asserts, fields } } => {
        let name = match eval(env, ars, *idx)? {
          Val::Prim(Prim::String(x)) => x,
          Val::Prim(_) | Val::Rec { .. } => return Err(EvalError::IncompatibleTypes),
        };
        let Some(&(_, body)) = fields.get(&name) else { return Err(EvalError::NoSuchFieldName) };
        let kind = RecValKind::Object { asserts: asserts.clone(), fields: fields.clone() };
        let this = Val::Rec { env: obj_env.clone(), kind };
        obj_env.insert(Id::SELF, Subst::Val(this));
        obj_env.insert(Id::SUPER, Subst::Val(Val::empty_object()));
        for assert in asserts {
          eval(&obj_env, ars, assert)?;
        }
        eval(&obj_env, ars, body)
      }
      Val::Rec { env: ary_env, kind: RecValKind::Array(elems) } => {
        let idx = match eval(env, ars, *idx)? {
          Val::Prim(Prim::Number(x)) => x,
          Val::Prim(_) | Val::Rec { .. } => return Err(EvalError::IncompatibleTypes),
        };
        let idx_floor = idx.floor();
        let diff = idx - idx_floor;
        if diff.abs() > EPSILON {
          return Err(EvalError::ArrayIdxNotInteger);
        }
        if idx_floor < 0.0 || idx_floor > f64::from(u32::MAX) {
          return Err(EvalError::ArrayIdxOutOfRange);
        }
        #[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
        let idx = idx_floor as u32;
        let Ok(idx) = usize::try_from(idx) else { return Err(EvalError::ArrayIdxOutOfRange) };
        match elems.get(idx) {
          Some(&elem) => eval(&ary_env, ars, elem),
          None => Err(EvalError::ArrayIdxOutOfRange),
        }
      }
      Val::Rec { .. } | Val::Prim(_) => Err(EvalError::IncompatibleTypes),
    },
    ExprData::Call { func, positional, named } => {
      let (func_env, mut params, body) = match eval(env, ars, *func)? {
        Val::Rec { env, kind: RecValKind::Function { params, body } } => (env, params, body),
        Val::Rec { .. } | Val::Prim(_) => return Err(EvalError::IncompatibleTypes),
      };
      if positional.len() + named.len() > params.len() {
        return Err(EvalError::TooManyArguments);
      }
      let mut provided = FxHashSet::<Id>::default();
      for ((id, param), &arg) in params.iter_mut().zip(positional) {
        *param = arg;
        assert!(provided.insert(*id), "duplicate function param should be forbidden by check");
      }
      for &(arg_name, arg) in named {
        if !provided.insert(arg_name) {
          return Err(EvalError::DuplicateArgument);
        }
        // we're getting a little fancy here. this iterates across the mutable params, and if we
        // could find a param whose name matches the arg's name, then this sets the param to that
        // arg and short circuits with true. note `==` with comparing the names and `=` with setting
        // the actual exprs. note the useage of `bool::then` with `find_map` and `is_none`.
        let failed_to_set_arg = params
          .iter_mut()
          .find_map(|(param_name, param)| (*param_name == arg_name).then(|| *param = arg))
          .is_none();
        if failed_to_set_arg {
          return Err(EvalError::NoSuchArgument);
        }
      }
      eval_local(&func_env, &params, ars, body)
    }
    ExprData::Id(id) => match env.get(*id) {
      Subst::Val(v) => Ok(v.clone()),
      Subst::Expr(env, expr) => eval(env, ars, *expr),
    },
    ExprData::Local { binds, body } => eval_local(env, binds, ars, *body),
    ExprData::If { cond, yes, no } => {
      let b = match eval(env, ars, *cond)? {
        Val::Prim(Prim::Bool(x)) => x,
        Val::Prim(_) | Val::Rec { .. } => return Err(EvalError::IncompatibleTypes),
      };
      let &expr = if b { yes } else { no };
      eval(env, ars, expr)
    }
    ExprData::BinaryOp { lhs, op, rhs } => match op {
      // plus
      BinaryOp::Plus => match (eval(env, ars, *lhs)?, eval(env, ars, *rhs)?) {
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
        _ => Err(EvalError::Todo),
      },
      // arithmetic
      BinaryOp::Star => float_op(env, ars, *lhs, *rhs, std::ops::Mul::mul),
      BinaryOp::Slash => float_op(env, ars, *lhs, *rhs, std::ops::Div::div),
      BinaryOp::Minus => float_op(env, ars, *lhs, *rhs, std::ops::Sub::sub),
      // bitwise
      BinaryOp::LtLt => int_op(env, ars, *lhs, *rhs, std::ops::Shl::shl),
      BinaryOp::GtGt => int_op(env, ars, *lhs, *rhs, std::ops::Shr::shr),
      BinaryOp::And => int_op(env, ars, *lhs, *rhs, std::ops::BitAnd::bitand),
      BinaryOp::Carat => int_op(env, ars, *lhs, *rhs, std::ops::BitXor::bitxor),
      BinaryOp::Bar => int_op(env, ars, *lhs, *rhs, std::ops::BitOr::bitor),
      // comparison
      BinaryOp::Lt => cmp_op(env, ars, *lhs, *rhs, Ordering::is_lt),
      BinaryOp::LtEq => cmp_op(env, ars, *lhs, *rhs, Ordering::is_le),
      BinaryOp::Gt => cmp_op(env, ars, *lhs, *rhs, Ordering::is_gt),
      BinaryOp::GtEq => cmp_op(env, ars, *lhs, *rhs, Ordering::is_ge),
      // logical
      BinaryOp::AndAnd => match eval(env, ars, *lhs)? {
        Val::Prim(Prim::Bool(true)) => eval(env, ars, *rhs),
        Val::Prim(Prim::Bool(false)) => Ok(Val::Prim(Prim::Bool(false))),
        Val::Prim(_) | Val::Rec { .. } => Err(EvalError::IncompatibleTypes),
      },
      BinaryOp::BarBar => match eval(env, ars, *lhs)? {
        Val::Prim(Prim::Bool(true)) => Ok(Val::Prim(Prim::Bool(true))),
        Val::Prim(Prim::Bool(false)) => eval(env, ars, *rhs),
        Val::Prim(_) | Val::Rec { .. } => Err(EvalError::IncompatibleTypes),
      },
    },
    ExprData::UnaryOp { .. } => Err(EvalError::Todo),
    ExprData::Function { params, body } => {
      let kind = RecValKind::Function { params: params.clone(), body: *body };
      Ok(Val::Rec { env: env.clone(), kind })
    }
    ExprData::Error(inner) => {
      let val = eval(env, ars, *inner)?;
      Err(EvalError::User(str_conv(val)))
    }
  }
}

fn float_pair(env: &Env, ars: &Arenas, a: Expr, b: Expr) -> Eval<[f64; 2]> {
  match (eval(env, ars, a)?, eval(env, ars, b)?) {
    (Val::Prim(Prim::Number(a)), Val::Prim(Prim::Number(b))) => Ok([a, b]),
    _ => Err(EvalError::IncompatibleTypes),
  }
}

fn float_op<F>(env: &Env, ars: &Arenas, lhs: Expr, rhs: Expr, f: F) -> Eval
where
  F: FnOnce(f64, f64) -> f64,
{
  let [lhs, rhs] = float_pair(env, ars, lhs, rhs)?;
  Ok(Val::Prim(Prim::Number(f(lhs, rhs))))
}

fn int_op<F>(env: &Env, ars: &Arenas, lhs: Expr, rhs: Expr, f: F) -> Eval
where
  F: FnOnce(i64, i64) -> i64,
{
  let nums = float_pair(env, ars, lhs, rhs)?;
  #[allow(clippy::cast_possible_truncation)]
  let [lhs, rhs] = nums.map(|x| x as i64);
  #[allow(clippy::cast_precision_loss)]
  let n = f(lhs, rhs) as f64;
  Ok(Val::Prim(Prim::Number(n)))
}

fn cmp_op<F>(env: &Env, ars: &Arenas, lhs: Expr, rhs: Expr, f: F) -> Eval
where
  F: FnOnce(Ordering) -> bool,
{
  let lhs = eval(env, ars, lhs)?;
  let rhs = eval(env, ars, rhs)?;
  let ord = cmp_val(ars, &lhs, &rhs)?;
  Ok(Val::Prim(Prim::Bool(f(ord))))
}

fn cmp_val(ars: &Arenas, lhs: &Val, rhs: &Val) -> Eval<Ordering> {
  match (lhs, rhs) {
    (Val::Prim(lhs), Val::Prim(rhs)) => match (lhs, rhs) {
      (Prim::String(lhs), Prim::String(rhs)) => Ok(ars.str.get(*lhs).cmp(ars.str.get(*rhs))),
      (Prim::Number(lhs), Prim::Number(rhs)) => {
        if lhs.is_nan() || rhs.is_nan() {
          Err(EvalError::CmpNan)
        } else if rhs.is_infinite() || lhs.is_infinite() {
          Err(EvalError::CmpInf)
        } else {
          Ok(lhs.partial_cmp(rhs).expect("not nan or inf"))
        }
      }
      _ => Err(EvalError::IncompatibleTypes),
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
              let lhs = eval(le, ars, lhs)?;
              let rhs = eval(re, ars, rhs)?;
              match cmp_val(ars, &lhs, &rhs)? {
                Ordering::Equal => {}
                ord => break ord,
              }
            }
          }
        };
        Ok(ord)
      }
      _ => Err(EvalError::IncompatibleTypes),
    },
    _ => Err(EvalError::IncompatibleTypes),
  }
}

fn eval_local(_: &Env, _: &[(Id, Expr)], _: &Arenas, _: Expr) -> Eval {
  Err(EvalError::Todo)
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
