//! The standard library for Jsonnet, implemented in Rust.

use crate::error::Result;
use crate::generated::{args, params};
use crate::{error, exec, mk_todo, std_lib_impl, Cx};
use finite_float::Float;
use jsonnet_expr::{Expr, ExprMust, Id, Prim, StdFn};
use jsonnet_val::jsonnet::{Env, Val};

pub(crate) fn get(
  cx: Cx<'_>,
  env: &Env,
  positional: &[Expr],
  named: &[(Id, Expr)],
  expr: ExprMust,
  std_fn: StdFn,
) -> Result<Val> {
  match std_fn {
    StdFn::abs => math_op(cx, env, positional, named, expr, f64::abs),
    StdFn::exp => math_op(cx, env, positional, named, expr, f64::exp),
    // TODO is it log2 or log10?
    StdFn::log => math_op(cx, env, positional, named, expr, f64::log2),
    StdFn::floor => math_op(cx, env, positional, named, expr, f64::floor),
    StdFn::ceil => math_op(cx, env, positional, named, expr, f64::ceil),
    StdFn::sqrt => math_op(cx, env, positional, named, expr, f64::sqrt),
    StdFn::sin => math_op(cx, env, positional, named, expr, f64::sin),
    StdFn::cos => math_op(cx, env, positional, named, expr, f64::cos),
    StdFn::tan => math_op(cx, env, positional, named, expr, f64::tan),
    StdFn::asin => math_op(cx, env, positional, named, expr, f64::asin),
    StdFn::acos => math_op(cx, env, positional, named, expr, f64::acos),
    StdFn::atan => math_op(cx, env, positional, named, expr, f64::atan),
    StdFn::round => math_op(cx, env, positional, named, expr, f64::round),
    StdFn::join => {
      let arguments = args::join(positional, named, expr)?;
      let sep = exec::get(cx, env, arguments.sep)?;
      let Val::Array(arr) = exec::get(cx, env, arguments.arr)? else {
        return Err(error::Error::Exec {
          expr: arguments.arr.unwrap_or(expr),
          kind: error::Kind::IncompatibleTypes,
        });
      };
      std_lib_impl::join(&sep, &arr, expr, cx)
    }
    StdFn::equals => {
      let arguments = args::equals(positional, named, expr)?;
      let lhs = exec::get(cx, env, arguments.x)?;
      let rhs = exec::get(cx, env, arguments.y)?;
      Ok(exec::eq_val(expr, cx, &lhs, &rhs)?.into())
    }
    _ => Err(mk_todo(expr, std_fn.as_static_str())),
  }
}

fn math_op(
  cx: Cx<'_>,
  env: &Env,
  positional: &[Expr],
  named: &[(Id, Expr)],
  expr: ExprMust,
  f: fn(f64) -> f64,
) -> Result<Val> {
  let arguments = params::x::get(positional, named, expr)?;
  let x = exec::get(cx, env, arguments.x)?;
  let x = get_num(&x, arguments.x.unwrap_or(expr))?;
  mk_num(f(x), expr)
}

pub(crate) fn get_num(v: &Val, expr: ExprMust) -> Result<f64> {
  match v {
    Val::Prim(Prim::Number(x)) => Ok(x.value()),
    _ => Err(error::Error::Exec { expr, kind: error::Kind::IncompatibleTypes }),
  }
}

pub(crate) fn mk_num(n: f64, expr: ExprMust) -> Result<Val> {
  match Float::try_from(n) {
    Ok(x) => Ok(x.into()),
    Err(e) => Err(error::Error::Exec { expr, kind: error::Kind::Infinite(e) }),
  }
}
