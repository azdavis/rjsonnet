//! TODO write doc

#![allow(non_snake_case)]

use jsonnet_expr::{Prim, Str};
use jsonnet_val::jsonnet::Val;

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
