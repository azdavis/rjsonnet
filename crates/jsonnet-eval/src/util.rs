//! Helper utilities.

use crate::error::{self, Result};
use jsonnet_expr::{ExprMust, Prim, StrArena};
use jsonnet_val::jsonnet::{Array, Val};

pub(crate) fn get_num(v: &Val, expr: ExprMust) -> Result<f64> {
  if let Val::Prim(Prim::Number(x)) = v {
    Ok(x.value())
  } else {
    Err(error::Error::Exec { expr, kind: error::Kind::IncompatibleTypes })
  }
}

pub(crate) fn mk_num(n: f64, expr: ExprMust) -> Result<Val> {
  match finite_float::Float::try_from(n) {
    Ok(x) => Ok(x.into()),
    Err(e) => Err(error::Error::Exec { expr, kind: error::Kind::Infinite(e) }),
  }
}

pub(crate) fn get_arr(v: &Val, expr: ExprMust) -> Result<&Array> {
  if let Val::Array(arr) = v {
    Ok(arr)
  } else {
    Err(error::Error::Exec { expr, kind: error::Kind::IncompatibleTypes })
  }
}

pub(crate) fn get_str<'a>(v: &'a Val, sa: &'a StrArena, expr: ExprMust) -> Result<&'a str> {
  if let Val::Prim(Prim::String(s)) = v {
    Ok(sa.get(s))
  } else {
    Err(error::Error::Exec { expr, kind: error::Kind::IncompatibleTypes })
  }
}
