//! The standard library for Jsonnet, implemented in Rust.

#![allow(non_snake_case)]

use crate::error::{self, Error, Result};
use crate::{exec, Cx};
use jsonnet_expr::{ExprMust, Prim, Str, Visibility};
use jsonnet_val::jsonnet::{Array, Fn, Object, Val};
use rustc_hash::FxHashSet;

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
    Val::Object(obj) => {
      Ok(obj.fields().iter().filter(|&&(_, vis, _)| vis != Visibility::Hidden).count())
    }
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

pub(crate) fn exp(x: f64) -> f64 {
  x.exp()
}

pub(crate) fn log(x: f64) -> f64 {
  x.ln()
}

pub(crate) fn abs(n: f64) -> f64 {
  n.abs()
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

pub(crate) fn equals(lhs: &Val, rhs: &Val, expr: ExprMust, cx: Cx<'_>) -> Result<bool> {
  exec::eq_val(expr, cx, lhs, rhs)
}

pub(crate) fn isEven(n: f64) -> bool {
  n.abs() % 2.0 == 0.0
}

#[expect(clippy::float_cmp)]
pub(crate) fn isOdd(n: f64) -> bool {
  n.abs() % 2.0 == 1.0
}

#[expect(clippy::float_cmp)]
pub(crate) fn isInteger(n: f64) -> bool {
  n.trunc() == n
}

pub(crate) fn isDecimal(n: f64) -> bool {
  !isInteger(n)
}

pub(crate) fn clamp(n: f64, min: f64, max: f64) -> f64 {
  n.clamp(min, max)
}

pub(crate) fn isEmpty(s: &str) -> bool {
  s.is_empty()
}

pub(crate) fn asciiUpper(s: &str) -> String {
  s.to_ascii_uppercase()
}

pub(crate) fn asciiLower(s: &str) -> String {
  s.to_ascii_lowercase()
}

pub(crate) fn strReplace(str: &str, from: &str, to: &str) -> String {
  str.replace(from, to)
}

pub(crate) fn substr(
  str: &str,
  from: usize,
  len: usize,
  expr: ExprMust,
  _: Cx<'_>,
) -> Result<String> {
  if from >= str.len() {
    return Err(error::Error::Exec { expr, kind: error::Kind::IdxOutOfRange(from) });
  }
  let Some(fst) = str.get(from..) else {
    return Err(error::Error::Exec { expr, kind: error::Kind::IdxNotUtf8Boundary(from) });
  };
  if fst.len() < len {
    Ok(fst.to_owned())
  } else {
    match fst.get(..len) {
      Some(x) => Ok(x.to_owned()),
      None => Err(error::Error::Exec { expr, kind: error::Kind::IdxNotUtf8Boundary(len) }),
    }
  }
}

pub(crate) fn startsWith(a: &str, b: &str) -> bool {
  a.starts_with(b)
}

pub(crate) fn endsWith(a: &str, b: &str) -> bool {
  a.ends_with(b)
}

pub(crate) fn stripChars(s: &str, cs: &str) -> String {
  let cs: FxHashSet<_> = cs.chars().collect();
  s.trim_matches(|c| cs.contains(&c)).to_owned()
}

pub(crate) fn lstripChars(s: &str, cs: &str) -> String {
  let cs: FxHashSet<_> = cs.chars().collect();
  s.trim_start_matches(|c| cs.contains(&c)).to_owned()
}

pub(crate) fn rstripChars(s: &str, cs: &str) -> String {
  let cs: FxHashSet<_> = cs.chars().collect();
  s.trim_end_matches(|c| cs.contains(&c)).to_owned()
}

pub(crate) fn xor(a: bool, b: bool) -> bool {
  a != b
}

pub(crate) fn xnor(a: bool, b: bool) -> bool {
  a == b
}

pub(crate) fn objectHas(o: &Object, f: &Str) -> bool {
  o.get_field(f).is_some_and(|(vis, _)| matches!(vis, Visibility::Default | Visibility::Visible))
}

pub(crate) fn objectHasAll(o: &Object, f: &Str) -> bool {
  o.get_field(f).is_some()
}

pub(crate) fn objectHasEx(o: &Object, f: &Str, hidden: bool) -> bool {
  if hidden {
    objectHasAll(o, f)
  } else {
    objectHas(o, f)
  }
}
