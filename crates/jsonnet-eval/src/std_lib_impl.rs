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

pub(crate) fn max(a: f64, b: f64) -> f64 {
  a.max(b)
}

pub(crate) fn min(a: f64, b: f64) -> f64 {
  a.min(b)
}

pub(crate) fn pow(x: f64, n: f64) -> f64 {
  x.powf(n)
}

pub(crate) fn abs(n: f64) -> f64 {
  n.abs()
}

pub(crate) fn exp(n: f64) -> f64 {
  n.exp()
}

/// TODO is it log2 or log10?
pub(crate) fn log(n: f64) -> f64 {
  n.log2()
}

pub(crate) fn floor(n: f64) -> f64 {
  n.floor()
}

pub(crate) fn ceil(n: f64) -> f64 {
  n.ceil()
}

pub(crate) fn sqrt(n: f64) -> f64 {
  n.sqrt()
}

pub(crate) fn sin(n: f64) -> f64 {
  n.sin()
}

pub(crate) fn cos(n: f64) -> f64 {
  n.cos()
}

pub(crate) fn tan(n: f64) -> f64 {
  n.tan()
}

pub(crate) fn asin(n: f64) -> f64 {
  n.asin()
}

pub(crate) fn acos(n: f64) -> f64 {
  n.acos()
}

pub(crate) fn atan(n: f64) -> f64 {
  n.atan()
}

pub(crate) fn round(n: f64) -> f64 {
  n.round()
}
