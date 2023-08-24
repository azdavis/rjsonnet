//! A lazy evaluator for jsonnet.
//!
//! From the [spec](https://jsonnet.org/ref/spec.html).

#![deny(clippy::pedantic, missing_debug_implementations, missing_docs, rust_2018_idioms)]
// TODO clean up allows
#![allow(dead_code, clippy::manual_let_else, clippy::too_many_lines)]

use la_arena::{Arena, Idx};
use rustc_hash::{FxHashMap, FxHashSet};
use std::cmp::Ordering;

#[derive(Debug, Clone, Copy)]
enum Prim {
  Null,
  Bool(bool),
  String(Str),
  Number(f64),
}

type Expr = Idx<ExprData>;
type ExprArena = Arena<ExprData>;

#[derive(Debug)]
enum ExprData {
  Prim(Prim),
  Object { asserts: Vec<Expr>, fields: Vec<(Expr, Visibility, Expr)> },
  ObjectComp { name: Expr, body: Expr, id: Id, ary: Expr },
  Array(Vec<Expr>),
  Subscript { on: Expr, idx: Expr },
  Call { func: Expr, positional: Vec<Expr>, named: Vec<(Id, Expr)> },
  Id(Id),
  Local { binds: Vec<(Id, Expr)>, body: Expr },
  If { cond: Expr, yes: Expr, no: Expr },
  BinaryOp { lhs: Expr, op: BinaryOp, rhs: Expr },
  UnaryOp { op: UnaryOp, inner: Expr },
  Function { params: Vec<(Id, Expr)>, body: Expr },
  Error(Expr),
}

#[derive(Debug, Clone, Copy)]
enum Visibility {
  Default,
  Hidden,
  Visible,
}

#[derive(Debug, Clone, Copy)]
enum BinaryOp {
  Star,
  Slash,
  Plus,
  Minus,
  LtLt,
  GtGt,
  Lt,
  LtEq,
  Gt,
  GtEq,
  And,
  Carat,
  Bar,
  AndAnd,
  BarBar,
}

#[derive(Debug, Clone, Copy)]
enum UnaryOp {
  Minus,
  Plus,
  Bang,
  Tilde,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Id {}

impl Id {
  const STD: Self = Self {};
  const SELF: Self = Self {};
  const SUPER: Self = Self {};
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Str {}

#[derive(Debug)]
struct StrArena {}

impl StrArena {
  fn insert(&mut self, _: String) -> Str {
    todo!()
  }

  fn get(&self, _: Str) -> &str {
    todo!()
  }
}

struct St {
  errors: Vec<(Expr, &'static str)>,
}

impl St {
  fn err(&mut self, e: Expr, s: &'static str) {
    self.errors.push((e, s));
  }
}

#[derive(Debug, Clone)]
struct Cx {
  store: FxHashSet<Id>,
}

impl Cx {
  fn insert(&mut self, id: Id) {
    self.store.insert(id);
  }

  fn contains(&self, id: Id) -> bool {
    self.store.contains(&id)
  }
}

struct Arenas {
  str: StrArena,
  expr: ExprArena,
}

fn check(st: &mut St, cx: &Cx, ars: &Arenas, expr: Expr) {
  match &ars.expr[expr] {
    ExprData::Prim(_) => {}
    ExprData::Object { asserts, fields } => {
      let cx_big = {
        let mut cx = cx.clone();
        cx.insert(Id::SELF);
        cx.insert(Id::SUPER);
        cx
      };
      let mut field_names = FxHashSet::<Str>::default();
      for &(name, _, body) in fields {
        check(st, cx, ars, name);
        check(st, &cx_big, ars, body);
        if let ExprData::Prim(Prim::String(s)) = ars.expr[name] {
          if !field_names.insert(s) {
            st.err(name, "duplicate field name");
          }
        }
      }
      for &cond in asserts {
        check(st, &cx_big, ars, cond);
      }
    }
    ExprData::ObjectComp { name, body, id, ary } => {
      check(st, cx, ars, *ary);
      let mut cx = cx.clone();
      cx.insert(*id);
      check(st, &cx, ars, *name);
      cx.insert(Id::SELF);
      cx.insert(Id::SUPER);
      check(st, &cx, ars, *body);
    }
    ExprData::Array(exprs) => {
      for &arg in exprs {
        check(st, cx, ars, arg);
      }
    }
    ExprData::Subscript { on, idx } => {
      check(st, cx, ars, *on);
      check(st, cx, ars, *idx);
    }
    ExprData::Call { func, positional, named } => {
      check(st, cx, ars, *func);
      for &arg in positional {
        check(st, cx, ars, arg);
      }
      let mut arg_names = FxHashSet::<Id>::default();
      for &(id, arg) in named {
        check(st, cx, ars, arg);
        if !arg_names.insert(id) {
          // TODO move err to the id, not the arg
          st.err(arg, "duplicate named argument");
        }
      }
    }
    ExprData::Id(id) => {
      if !cx.contains(*id) {
        st.err(expr, "identifier not in scope");
      }
    }
    // turns out these are exactly the same
    ExprData::Local { binds, body } | ExprData::Function { params: binds, body } => {
      let mut cx = cx.clone();
      let mut bound_names = FxHashSet::<Id>::default();
      for &(id, rhs) in binds {
        cx.insert(id);
        if !bound_names.insert(id) {
          // TODO move err to the id, not the rhs
          st.err(rhs, "duplicate binding");
        }
      }
      for &(_, rhs) in binds {
        check(st, &cx, ars, rhs);
      }
      check(st, &cx, ars, *body);
    }
    ExprData::If { cond, yes, no } => {
      check(st, cx, ars, *cond);
      check(st, cx, ars, *yes);
      check(st, cx, ars, *no);
    }
    ExprData::BinaryOp { lhs, rhs, .. } => {
      check(st, cx, ars, *lhs);
      check(st, cx, ars, *rhs);
    }
    ExprData::UnaryOp { inner, .. } | ExprData::Error(inner) => {
      check(st, cx, ars, *inner);
    }
  }
}

#[derive(Debug, Default, Clone)]
struct Env {}

impl Env {
  fn insert(&mut self, _: Id, _: Subst) {
    todo!()
  }

  fn get(&self, _: Id) -> &Subst {
    todo!()
  }
}

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
enum Val {
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
enum RecValKind {
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

enum EvalError {
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
}

type Eval<T = Val> = Result<T, EvalError>;

const EPSILON: f64 = 0.0001;

/// TODO implement a cache on expr to avoid re-computing lazy exprs? but we would also need to
/// consider the env in which the expr is evaluated
fn eval(env: &Env, ars: &Arenas, expr: Expr) -> Eval {
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
            let body = subst(elem, *id, ars, *body);
            if fields.insert(s, (Visibility::Default, body)).is_some() {
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
        let &(_, body) = match fields.get(&name) {
          Some(x) => x,
          None => return Err(EvalError::NoSuchFieldName),
        };
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
        let idx = match usize::try_from(idx) {
          Ok(x) => x,
          Err(_) => return Err(EvalError::ArrayIdxOutOfRange),
        };
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
        _ => todo!(),
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
    ExprData::UnaryOp { .. } => todo!(),
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

/// subst(e,x,ars,body) = [e/x]body
fn subst(_: Expr, _: Id, _: &Arenas, _: Expr) -> Expr {
  todo!("subst: might need to rethink this approach")
}

fn eval_local(_: &Env, _: &[(Id, Expr)], _: &Arenas, _: Expr) -> Eval {
  todo!()
}

#[allow(clippy::needless_pass_by_value)]
fn str_conv(val: Val) -> Str {
  match val {
    Val::Prim(Prim::String(s)) => s,
    _ => todo!(),
  }
}

fn str_concat(_: Str, _: Str) -> Str {
  todo!()
}
