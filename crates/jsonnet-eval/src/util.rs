//! Helper utilities.

use crate::error::{self, Result};
use jsonnet_expr::{ExprMust, Prim, StrArena};
use jsonnet_val::jsonnet::{Array, Object, Val};

pub(crate) fn get_num(v: &Val, expr: ExprMust) -> Result<f64> {
  if let Val::Prim(Prim::Number(x)) = v {
    Ok(x.value())
  } else {
    Err(error::Error::Exec { expr, kind: error::Kind::IncompatibleTypes })
  }
}

pub(crate) fn get_bool(v: &Val, expr: ExprMust) -> Result<bool> {
  if let Val::Prim(Prim::Bool(x)) = v {
    Ok(*x)
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

pub(crate) fn get_obj(v: &Val, expr: ExprMust) -> Result<&Object> {
  if let Val::Object(o) = v {
    Ok(o)
  } else {
    Err(error::Error::Exec { expr, kind: error::Kind::IncompatibleTypes })
  }
}

pub(crate) fn get_str(v: &Val, expr: ExprMust) -> Result<&jsonnet_expr::Str> {
  if let Val::Prim(Prim::String(s)) = v {
    Ok(s)
  } else {
    Err(error::Error::Exec { expr, kind: error::Kind::IncompatibleTypes })
  }
}

pub(crate) fn mk_str(sa: &mut StrArena, s: String) -> Val {
  sa.str(s.into_boxed_str()).into()
}

#[expect(clippy::float_cmp, clippy::cast_possible_truncation, clippy::cast_sign_loss)]
pub(crate) fn get_uint(n: f64, expr: ExprMust) -> Result<usize> {
  if n >= 0.0 && n.trunc() == n {
    Ok(n as usize)
  } else {
    Err(error::Error::Exec { expr, kind: error::Kind::IncompatibleTypes })
  }
}
