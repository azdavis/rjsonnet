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
    StdFn::type_ => {
      let arguments = args::type_(positional, named, expr)?;
      let x = exec::get(cx, env, arguments.x)?;
      let res = std_lib_impl::type_(&x);
      Ok(res.into())
    }
    StdFn::isArray => {
      let arguments = args::isArray(positional, named, expr)?;
      let v = exec::get(cx, env, arguments.v)?;
      let res = std_lib_impl::isArray(&v);
      Ok(res.into())
    }
    StdFn::isBoolean => {
      let arguments = args::isBoolean(positional, named, expr)?;
      let v = exec::get(cx, env, arguments.v)?;
      let res = std_lib_impl::isBoolean(&v);
      Ok(res.into())
    }
    StdFn::isFunction => {
      let arguments = args::isFunction(positional, named, expr)?;
      let v = exec::get(cx, env, arguments.v)?;
      let res = std_lib_impl::isFunction(&v);
      Ok(res.into())
    }
    StdFn::isNumber => {
      let arguments = args::isNumber(positional, named, expr)?;
      let v = exec::get(cx, env, arguments.v)?;
      let res = std_lib_impl::isNumber(&v);
      Ok(res.into())
    }
    StdFn::isObject => {
      let arguments = args::isObject(positional, named, expr)?;
      let v = exec::get(cx, env, arguments.v)?;
      let res = std_lib_impl::isObject(&v);
      Ok(res.into())
    }
    StdFn::isString => {
      let arguments = args::isString(positional, named, expr)?;
      let v = exec::get(cx, env, arguments.v)?;
      let res = std_lib_impl::isString(&v);
      Ok(res.into())
    }
    StdFn::length => {
      let arguments = args::length(positional, named, expr)?;
      let x = exec::get(cx, env, arguments.x)?;
      let ret = std_lib_impl::length(&x, arguments.x.unwrap_or(expr), cx)?;
      Ok(Float::from(ret).into())
    }
    StdFn::abs => math_op(cx, env, positional, named, expr, f64::abs),
    StdFn::sign => {
      let arguments = args::sign(positional, named, expr)?;
      let n = exec::get(cx, env, arguments.n)?;
      let n = get_num(&n, arguments.n.unwrap_or(expr))?;
      let res = std_lib_impl::sign(n);
      mk_num(res, expr)
    }
    StdFn::max => {
      let arguments = args::max(positional, named, expr)?;
      let a = exec::get(cx, env, arguments.a)?;
      let b = exec::get(cx, env, arguments.b)?;
      let a = get_num(&a, arguments.a.unwrap_or(expr))?;
      let b = get_num(&b, arguments.b.unwrap_or(expr))?;
      let res = std_lib_impl::max(a, b);
      mk_num(res, expr)
    }
    StdFn::min => {
      let arguments = args::min(positional, named, expr)?;
      let a = exec::get(cx, env, arguments.a)?;
      let b = exec::get(cx, env, arguments.b)?;
      let a = get_num(&a, arguments.a.unwrap_or(expr))?;
      let b = get_num(&b, arguments.b.unwrap_or(expr))?;
      let res = std_lib_impl::min(a, b);
      mk_num(res, expr)
    }
    StdFn::pow => {
      let arguments = args::pow(positional, named, expr)?;
      let x = exec::get(cx, env, arguments.x)?;
      let n = exec::get(cx, env, arguments.n)?;
      let x = get_num(&x, arguments.x.unwrap_or(expr))?;
      let n = get_num(&n, arguments.n.unwrap_or(expr))?;
      let res = std_lib_impl::pow(x, n);
      mk_num(res, expr)
    }
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

fn get_num(v: &Val, expr: ExprMust) -> Result<f64> {
  match v {
    Val::Prim(Prim::Number(x)) => Ok(x.value()),
    _ => Err(error::Error::Exec { expr, kind: error::Kind::IncompatibleTypes }),
  }
}

fn mk_num(n: f64, expr: ExprMust) -> Result<Val> {
  match Float::try_from(n) {
    Ok(x) => Ok(x.into()),
    Err(e) => Err(error::Error::Exec { expr, kind: error::Kind::Infinite(e) }),
  }
}
