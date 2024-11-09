//! TODO write doc

#![allow(non_snake_case)]

use crate::error::{self, Error, Result};
use crate::Cx;
use jsonnet_expr::{ExprMust, Prim, Str};
use jsonnet_val::jsonnet::{Fn, Val};

pub(crate) fn type_(x: &Val) -> Str {
  match x {
    Val::Prim(prim) => match prim {
      Prim::Null => Str::null,
      Prim::Bool(_) => Str::boolean,
      Prim::String(_) => Str::string,
      Prim::Number(_) => Str::number,
    },
    Val::Object(_) => Str::object,
    Val::Array(_) => Str::array,
    Val::Fn(_) => Str::function,
  }
}

pub(crate) fn isArray(v: &Val) -> bool {
  matches!(v, Val::Array(_))
}

pub(crate) fn isBoolean(v: &Val) -> bool {
  matches!(v, Val::Prim(Prim::Bool(_)))
}

pub(crate) fn isFunction(v: &Val) -> bool {
  matches!(v, Val::Fn(_))
}

pub(crate) fn isNumber(v: &Val) -> bool {
  matches!(v, Val::Prim(Prim::Number(_)))
}

pub(crate) fn isObject(v: &Val) -> bool {
  matches!(v, Val::Object(_))
}

pub(crate) fn isString(v: &Val) -> bool {
  matches!(v, Val::Prim(Prim::String(_)))
}

pub(crate) fn length(x: &Val, expr: ExprMust, cx: Cx<'_>) -> Result<usize> {
  match x {
    Val::Prim(prim) => match prim {
      Prim::Null | Prim::Bool(_) | Prim::Number(_) => {
        Err(Error::Exec { expr, kind: error::Kind::IncompatibleTypes })
      }
      // we want "number of codepoints", NOT byte length.
      Prim::String(s) => Ok(cx.str_ar.get(s).chars().count()),
    },
    Val::Object(obj) => Ok(obj.fields().len()),
    Val::Array(arr) => Ok(arr.len()),
    Val::Fn(Fn::Regular(func)) => Ok(func.params.iter().filter(|(_, d)| d.is_none()).count()),
    Val::Fn(Fn::Std(func)) => Ok(func.required_params_count()),
  }
}
