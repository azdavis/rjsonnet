//! TODO write doc

#![allow(non_snake_case)]

use crate::error::{self, Error, Result};
use crate::{exec, Cx};
use jsonnet_expr::{ExprMust, Prim, Str};
use jsonnet_val::jsonnet::{Array, Fn, Val};

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

pub(crate) fn join(sep: &Val, arr: &Array, expr: ExprMust, cx: Cx<'_>) -> Result<Val> {
  match sep {
    Val::Prim(Prim::String(sep)) => {
      let mut ret = String::new();
      let sep = cx.str_ar.get(sep);
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

pub(crate) fn sign(n: f64) -> f64 {
  if n == 0.0 {
    0.0
  } else if n.is_sign_positive() {
    1.0
  } else {
    -1.0
  }
}
