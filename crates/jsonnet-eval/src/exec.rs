//! Executing Jsonnet expression to produce Jsonnet values.

use crate::error::{self, Result};
use crate::manifest;
use crate::val::jsonnet::{Array, Env, Object, StdFn, Val};
use jsonnet_expr::{
  Arenas, BinaryOp, Expr, ExprData, ExprMust, Id, Number, Prim, Str, StrArena, Visibility,
};
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
pub fn get(cx: Cx<'_>, ars: &Arenas, expr: Expr) -> Result<Val> {
  // TODO implement a cache on expr to avoid re-computing lazy exprs? but we would also need to
  // consider the env in which the expr is executed
  let expr = expr.expect("no expr");
  let mk_error = |kind: error::Kind| Err(error::Error::Exec { expr, kind });
  match &ars.expr[expr] {
    ExprData::Prim(p) => Ok(Val::Prim(p.clone())),
    ExprData::Object { asserts, fields } => {
      let mut named_fields = FxHashMap::<Str, (Visibility, Expr)>::default();
      for &(key, hid, val) in fields {
        match get(cx, ars, key)? {
          Val::Prim(Prim::String(s)) => {
            if named_fields.insert(s, (hid, val)).is_some() {
              return mk_error(error::Kind::DuplicateField);
            }
          }
          Val::Prim(Prim::Null) => {}
          _ => return mk_error(error::Kind::IncompatibleTypes),
        }
      }

      Ok(Val::Object(Object::new(cx.env.clone(), asserts.clone(), named_fields)))
    }
    ExprData::ObjectComp { name, body, id, ary } => {
      let Val::Array(array) = get(cx, ars, *ary)? else {
        return mk_error(error::Kind::IncompatibleTypes);
      };
      let mut fields = FxHashMap::<Str, (Visibility, Expr)>::default();
      for (part_env, elem) in array.iter() {
        let mut env = cx.env.clone();
        env.insert(*id, part_env.clone(), elem);
        match get(cx.with_env(&env), ars, *name)? {
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
      Ok(Val::Object(Object::new(cx.env.clone(), Vec::new(), fields)))
    }
    ExprData::Array(elems) => Ok(Val::Array(Array::new(cx.env.clone(), elems.clone()))),
    ExprData::Subscript { on, idx } => match get(cx, ars, *on)? {
      Val::Object(object) => {
        let Val::Prim(Prim::String(name)) = get(cx, ars, *idx)? else {
          return mk_error(error::Kind::IncompatibleTypes);
        };
        let Some((env, _, body)) = object.get_field(&name) else {
          return mk_error(error::Kind::NoSuchFieldName);
        };
        for (env, assert) in object.asserts() {
          get(cx.with_env(env), ars, assert)?;
        }
        get(cx.with_env(env), ars, body)
      }
      Val::Array(array) => {
        let Val::Prim(Prim::Number(idx)) = get(cx, ars, *idx)? else {
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
        match array.get(idx) {
          Some((env, elem)) => get(cx.with_env(env), ars, elem),
          None => mk_error(error::Kind::ArrayIdxOutOfRange),
        }
      }
      _ => mk_error(error::Kind::IncompatibleTypes),
    },
    ExprData::Call { func, positional, named } => match get(cx, ars, *func)? {
      Val::Function { env: func_env, mut params, body } => {
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
        let env = add_binds(&func_env, &params);
        get(cx.with_env(&env), ars, body)
      }
      Val::StdFn(std_val) => match std_val {
        StdFn::Cmp => {
          if !named.is_empty() {
            return mk_error(error::Kind::StdFuncNamedArgs);
          }
          let [lhs, rhs] = positional[..] else {
            return mk_error(error::Kind::StdFuncWrongNumArgs(2, positional.len()));
          };
          cmp_op(expr, cx, ars, lhs, rhs, |ord| {
            let num = match ord {
              Ordering::Less => Number::negative_one(),
              Ordering::Equal => Number::positive_zero(),
              Ordering::Greater => Number::positive_one(),
            };
            Prim::Number(num)
          })
        }
        StdFn::Equals => {
          if !named.is_empty() {
            return mk_error(error::Kind::StdFuncNamedArgs);
          }
          let [lhs, rhs] = positional[..] else {
            return mk_error(error::Kind::StdFuncWrongNumArgs(2, positional.len()));
          };
          cmp_bool_op(expr, cx, ars, lhs, rhs, Ordering::is_eq)
        }
      },
      _ => mk_error(error::Kind::IncompatibleTypes),
    },
    ExprData::Id(id) => {
      if *id == Id::SELF {
        return Ok(Val::Object(cx.this.clone()));
      }
      if *id == Id::SUPER {
        return Ok(Val::Object(cx.this.parent().expect("invalid `super`").clone()));
      }
      if *id == Id::DOLLAR {
        return Ok(Val::Object(cx.this.root().expect("invalid `$`").clone()));
      }
      let (env, expr) = cx.env.get(*id);
      get(cx.with_env(env), ars, expr)
    }
    ExprData::Local { binds, body } => {
      let env = add_binds(cx.env, binds);
      get(cx.with_env(&env), ars, *body)
    }
    ExprData::If { cond, yes, no } => {
      let Val::Prim(Prim::Bool(b)) = get(cx, ars, *cond)? else {
        return mk_error(error::Kind::IncompatibleTypes);
      };
      let &expr = if b { yes } else { no };
      get(cx, ars, expr)
    }
    ExprData::BinaryOp { lhs, op, rhs } => match op {
      // add
      BinaryOp::Add => match (get(cx, ars, *lhs)?, get(cx, ars, *rhs)?) {
        (Val::Prim(Prim::String(lhs)), rhs) => {
          let rhs = str_conv(cx.this, ars, rhs)?;
          Ok(Val::Prim(Prim::String(str_concat(&ars.str, &lhs, &rhs))))
        }
        (lhs, Val::Prim(Prim::String(rhs))) => {
          let lhs = str_conv(cx.this, ars, lhs)?;
          Ok(Val::Prim(Prim::String(str_concat(&ars.str, &lhs, &rhs))))
        }
        (Val::Prim(Prim::Number(lhs)), Val::Prim(Prim::Number(rhs))) => {
          let n = match Number::try_from(lhs.value() + rhs.value()) {
            Ok(n) => n,
            Err(inf) => return mk_error(error::Kind::Infinite(inf)),
          };
          Ok(Val::Prim(Prim::Number(n)))
        }
        (Val::Array(mut lhs), Val::Array(mut rhs)) => {
          lhs.append(&mut rhs);
          Ok(Val::Array(lhs))
        }
        (Val::Object(lhs), Val::Object(mut rhs)) => {
          rhs.set_parent_to(lhs);
          Ok(Val::Object(rhs))
        }
        _ => mk_error(error::Kind::IncompatibleTypes),
      },
      // arithmetic
      BinaryOp::Mul => float_op(expr, cx, ars, *lhs, *rhs, std::ops::Mul::mul),
      BinaryOp::Div => float_op(expr, cx, ars, *lhs, *rhs, std::ops::Div::div),
      BinaryOp::Sub => float_op(expr, cx, ars, *lhs, *rhs, std::ops::Sub::sub),
      // bitwise
      BinaryOp::Shl => int_op(expr, cx, ars, *lhs, *rhs, std::ops::Shl::shl),
      BinaryOp::Shr => int_op(expr, cx, ars, *lhs, *rhs, std::ops::Shr::shr),
      BinaryOp::BitAnd => int_op(expr, cx, ars, *lhs, *rhs, std::ops::BitAnd::bitand),
      BinaryOp::BitXor => int_op(expr, cx, ars, *lhs, *rhs, std::ops::BitXor::bitxor),
      BinaryOp::BitOr => int_op(expr, cx, ars, *lhs, *rhs, std::ops::BitOr::bitor),
      // comparison
      BinaryOp::Lt => cmp_bool_op(expr, cx, ars, *lhs, *rhs, Ordering::is_lt),
      BinaryOp::LtEq => cmp_bool_op(expr, cx, ars, *lhs, *rhs, Ordering::is_le),
      BinaryOp::Gt => cmp_bool_op(expr, cx, ars, *lhs, *rhs, Ordering::is_gt),
      BinaryOp::GtEq => cmp_bool_op(expr, cx, ars, *lhs, *rhs, Ordering::is_ge),
    },
    ExprData::UnaryOp { op, inner } => {
      let inner = get(cx, ars, *inner)?;
      match op {
        jsonnet_expr::UnaryOp::Neg => {
          if let Val::Prim(Prim::Number(n)) = inner {
            Ok(Val::Prim(Prim::Number(-n)))
          } else {
            mk_error(error::Kind::IncompatibleTypes)
          }
        }
        jsonnet_expr::UnaryOp::Pos => {
          if matches!(inner, Val::Prim(Prim::Number(_))) {
            Ok(inner)
          } else {
            mk_error(error::Kind::IncompatibleTypes)
          }
        }
        jsonnet_expr::UnaryOp::LogicalNot => {
          if let Val::Prim(Prim::Bool(b)) = inner {
            Ok(Val::Prim(Prim::Bool(!b)))
          } else {
            mk_error(error::Kind::IncompatibleTypes)
          }
        }
        jsonnet_expr::UnaryOp::BitNot => {
          if let Val::Prim(Prim::Number(_)) = inner {
            mk_error(error::Kind::Todo("bitwise not"))
          } else {
            mk_error(error::Kind::IncompatibleTypes)
          }
        }
      }
    }
    ExprData::Function { params, body } => {
      Ok(Val::Function { env: cx.env.clone(), params: params.clone(), body: *body })
    }
    ExprData::Error(inner) => {
      let val = get(cx, ars, *inner)?;
      let msg = str_conv(cx.this, ars, val)?;
      mk_error(error::Kind::User(msg))
    }
    ExprData::Import { .. } => todo!("Import"),
  }
}

#[derive(Debug, Clone, Copy)]
pub struct Cx<'a> {
  env: &'a Env,
  this: &'a Object,
}

impl<'a> Cx<'a> {
  /// Returns a new Cx.
  #[must_use]
  pub fn new(env: &'a Env, this: &'a Object) -> Cx<'a> {
    Cx { env, this }
  }

  pub(crate) fn this(self) -> &'a Object {
    self.this
  }

  fn with_env(self, env: &'a Env) -> Cx<'a> {
    Cx::new(env, self.this)
  }
}

fn number_pair(expr: ExprMust, cx: Cx<'_>, ars: &Arenas, a: Expr, b: Expr) -> Result<[Number; 2]> {
  match (get(cx, ars, a)?, get(cx, ars, b)?) {
    (Val::Prim(Prim::Number(a)), Val::Prim(Prim::Number(b))) => Ok([a, b]),
    _ => Err(error::Error::Exec { expr, kind: error::Kind::IncompatibleTypes }),
  }
}

fn float_op<F>(expr: ExprMust, cx: Cx<'_>, ars: &Arenas, lhs: Expr, rhs: Expr, f: F) -> Result<Val>
where
  F: FnOnce(f64, f64) -> f64,
{
  let [lhs, rhs] = number_pair(expr, cx, ars, lhs, rhs)?;
  let n = match Number::try_from(f(lhs.value(), rhs.value())) {
    Ok(n) => n,
    Err(inf) => return Err(error::Error::Exec { expr, kind: error::Kind::Infinite(inf) }),
  };
  Ok(Val::Prim(Prim::Number(n)))
}

fn int_op<F>(expr: ExprMust, cx: Cx<'_>, ars: &Arenas, lhs: Expr, rhs: Expr, f: F) -> Result<Val>
where
  F: FnOnce(i64, i64) -> i64,
{
  let ns = number_pair(expr, cx, ars, lhs, rhs)?;
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

fn cmp_op<F>(expr: ExprMust, cx: Cx<'_>, ars: &Arenas, lhs: Expr, rhs: Expr, f: F) -> Result<Val>
where
  F: FnOnce(Ordering) -> Prim,
{
  let lhs = get(cx, ars, lhs)?;
  let rhs = get(cx, ars, rhs)?;
  let ord = cmp_val(expr, cx.this, ars, &lhs, &rhs)?;
  Ok(Val::Prim(f(ord)))
}

fn cmp_bool_op<F>(
  expr: ExprMust,
  cx: Cx<'_>,
  ars: &Arenas,
  lhs: Expr,
  rhs: Expr,
  f: F,
) -> Result<Val>
where
  F: FnOnce(Ordering) -> bool,
{
  cmp_op(expr, cx, ars, lhs, rhs, |x| Prim::Bool(f(x)))
}

fn cmp_val(expr: ExprMust, this: &Object, ars: &Arenas, lhs: &Val, rhs: &Val) -> Result<Ordering> {
  match (lhs, rhs) {
    (Val::Prim(lhs), Val::Prim(rhs)) => match (lhs, rhs) {
      (Prim::String(lhs), Prim::String(rhs)) => Ok(ars.str.get(lhs).cmp(ars.str.get(rhs))),
      (Prim::Number(lhs), Prim::Number(rhs)) => Ok(lhs.cmp(rhs)),
      _ => Err(error::Error::Exec { expr, kind: error::Kind::IncompatibleTypes }),
    },
    (Val::Array(lhs), Val::Array(rhs)) => {
      let mut lhs = lhs.iter();
      let mut rhs = rhs.iter();
      let ord = loop {
        match (lhs.next(), rhs.next()) {
          (None, Some(_)) => break Ordering::Less,
          (None, None) => break Ordering::Equal,
          (Some(_), None) => break Ordering::Greater,
          (Some((le, lhs)), Some((re, rhs))) => {
            let lhs = get(Cx::new(le, this), ars, lhs)?;
            let rhs = get(Cx::new(re, this), ars, rhs)?;
            match cmp_val(expr, this, ars, &lhs, &rhs)? {
              Ordering::Equal => {}
              ord => break ord,
            }
          }
        }
      };
      Ok(ord)
    }
    _ => Err(error::Error::Exec { expr, kind: error::Kind::IncompatibleTypes }),
  }
}

fn add_binds(env: &Env, binds: &[(Id, Expr)]) -> Env {
  let mut ret = env.clone();
  for &(id, expr) in binds {
    ret.insert(id, env.clone(), expr);
  }
  ret
}

fn str_conv(this: &Object, ars: &Arenas, val: Val) -> Result<Str> {
  if let Val::Prim(Prim::String(s)) = val {
    Ok(s)
  } else {
    let json = manifest::get(ars, this, val)?;
    let string = json.display(&ars.str).to_string();
    Ok(ars.str.str_shared(string.into_boxed_str()))
  }
}

fn str_concat(ar: &StrArena, lhs: &Str, rhs: &Str) -> Str {
  let lhs = ar.get(lhs);
  let rhs = ar.get(rhs);
  let both = format!("{lhs}{rhs}").into_boxed_str();
  ar.str_shared(both)
}
