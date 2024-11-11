//! The standard library for Jsonnet, implemented in Rust.

use crate::error::Result;
use crate::generated::{args, params};
use crate::{error, exec, mk_todo, std_lib_impl, Cx};
use finite_float::Float;
use jsonnet_expr::{Expr, ExprMust, Id, Prim, StdFn};
use jsonnet_val::jsonnet::{Array, Env, Val};

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
      let ret = std_lib_impl::type_(&x);
      Ok(Val::Prim(Prim::String(ret)))
    }
    StdFn::isArray => {
      let arguments = args::isArray(positional, named, expr)?;
      let v = exec::get(cx, env, arguments.v)?;
      let res = std_lib_impl::isArray(&v);
      Ok(Val::Prim(Prim::Bool(res)))
    }
    StdFn::isBoolean => {
      let arguments = args::isBoolean(positional, named, expr)?;
      let v = exec::get(cx, env, arguments.v)?;
      let res = std_lib_impl::isBoolean(&v);
      Ok(Val::Prim(Prim::Bool(res)))
    }
    StdFn::isFunction => {
      let arguments = args::isFunction(positional, named, expr)?;
      let v = exec::get(cx, env, arguments.v)?;
      let res = std_lib_impl::isFunction(&v);
      Ok(Val::Prim(Prim::Bool(res)))
    }
    StdFn::isNumber => {
      let arguments = args::isNumber(positional, named, expr)?;
      let v = exec::get(cx, env, arguments.v)?;
      let res = std_lib_impl::isNumber(&v);
      Ok(Val::Prim(Prim::Bool(res)))
    }
    StdFn::isObject => {
      let arguments = args::isObject(positional, named, expr)?;
      let v = exec::get(cx, env, arguments.v)?;
      let res = std_lib_impl::isObject(&v);
      Ok(Val::Prim(Prim::Bool(res)))
    }
    StdFn::isString => {
      let arguments = args::isString(positional, named, expr)?;
      let v = exec::get(cx, env, arguments.v)?;
      let res = std_lib_impl::isString(&v);
      Ok(Val::Prim(Prim::Bool(res)))
    }
    StdFn::length => {
      let arguments = args::length(positional, named, expr)?;
      let x = exec::get(cx, env, arguments.x)?;
      let ret = std_lib_impl::length(&x, arguments.x.unwrap_or(expr), cx)?;
      Ok(Val::Prim(Prim::Number(Float::from(ret))))
    }
    StdFn::abs => math_op(cx, env, positional, named, expr, f64::abs),
    StdFn::sign => {
      let arguments = args::sign(positional, named, expr)?;
      let n = exec::get(cx, env, arguments.n)?;
      let val = get_num(&n, arguments.n.unwrap_or(expr))?;
      let res = if val == 0.0 {
        Float::positive_zero()
      } else if val.is_sign_positive() {
        Float::positive_one()
      } else {
        Float::negative_one()
      };
      Ok(Val::Prim(Prim::Number(res)))
    }
    StdFn::max => {
      let arguments = args::max(positional, named, expr)?;
      let a = exec::get(cx, env, arguments.a)?;
      let b = exec::get(cx, env, arguments.b)?;
      let a = get_num(&a, arguments.a.unwrap_or(expr))?;
      let b = get_num(&b, arguments.b.unwrap_or(expr))?;
      let res = a.max(b);
      mk_num(res, expr)
    }
    StdFn::min => {
      let arguments = args::min(positional, named, expr)?;
      let a = exec::get(cx, env, arguments.a)?;
      let b = exec::get(cx, env, arguments.b)?;
      let a = get_num(&a, arguments.a.unwrap_or(expr))?;
      let b = get_num(&b, arguments.b.unwrap_or(expr))?;
      let res = a.min(b);
      mk_num(res, expr)
    }
    StdFn::pow => {
      let arguments = args::pow(positional, named, expr)?;
      let x = exec::get(cx, env, arguments.x)?;
      let n = exec::get(cx, env, arguments.n)?;
      let x = get_num(&x, arguments.x.unwrap_or(expr))?;
      let n = get_num(&n, arguments.n.unwrap_or(expr))?;
      let res = x.powf(n);
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
      match sep {
        Val::Prim(Prim::String(sep)) => {
          let mut ret = String::new();
          let sep = cx.str_ar.get(&sep);
          let mut first = true;
          for (env, elem) in arr.iter() {
            if !first {
              ret.push_str(sep);
            };
            first = false;
            let Val::Prim(Prim::String(elem)) = exec::get(cx, env, elem)? else {
              return Err(error::Error::Exec {
                expr: elem.unwrap_or(expr),
                kind: error::Kind::IncompatibleTypes,
              });
            };
            ret.push_str(cx.str_ar.get(&elem));
          }
          Ok(Val::Prim(Prim::String(cx.str_ar.str_shared(ret.into_boxed_str()))))
        }
        Val::Array(sep) => {
          let mut ret = Array::default();
          let mut first = true;
          for (env, elem) in arr.iter() {
            if !first {
              ret.append(&mut sep.clone());
            };
            first = false;
            let Val::Array(mut elem) = exec::get(cx, env, elem)? else {
              return Err(error::Error::Exec {
                expr: elem.unwrap_or(expr),
                kind: error::Kind::IncompatibleTypes,
              });
            };
            ret.append(&mut elem);
          }
          Ok(Val::Array(ret))
        }
        Val::Prim(_) | Val::Object(_) | Val::Fn(_) => {
          Err(error::Error::Exec { expr, kind: error::Kind::IncompatibleTypes })
        }
      }
    }
    StdFn::equals => {
      let arguments = args::equals(positional, named, expr)?;
      let lhs = exec::get(cx, env, arguments.x)?;
      let rhs = exec::get(cx, env, arguments.y)?;
      Ok(Val::Prim(Prim::Bool(exec::eq_val(expr, cx, &lhs, &rhs)?)))
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
    Ok(x) => Ok(Val::Prim(Prim::Number(x))),
    Err(e) => Err(error::Error::Exec { expr, kind: error::Kind::Infinite(e) }),
  }
}
