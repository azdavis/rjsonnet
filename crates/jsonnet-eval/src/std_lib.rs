//! The standard library for Jsonnet, implemented in Rust.

use crate::error::Result;
use crate::generated::args;
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
    _ => Err(mk_todo(expr, std_fn.as_static_str())),
  }
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
