//! Helper utilities.

use crate::error::{self, Result};
use jsonnet_expr::{ExprMust, Prim};
use jsonnet_val::jsonnet::{Array, Val};

pub(crate) fn get_num(v: &Val, expr: ExprMust) -> Result<f64> {
  match v {
    Val::Prim(Prim::Number(x)) => Ok(x.value()),
    _ => Err(error::Error::Exec { expr, kind: error::Kind::IncompatibleTypes }),
  }
}

pub(crate) fn mk_num(n: f64, expr: ExprMust) -> Result<Val> {
  match finite_float::Float::try_from(n) {
    Ok(x) => Ok(x.into()),
    Err(e) => Err(error::Error::Exec { expr, kind: error::Kind::Infinite(e) }),
  }
}

pub(crate) fn get_arr(v: &Val, expr: ExprMust) -> Result<&Array> {
  match v {
    Val::Array(arr) => Ok(arr),
    _ => Err(error::Error::Exec { expr, kind: error::Kind::IncompatibleTypes }),
  }
}
