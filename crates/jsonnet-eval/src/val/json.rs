//! JSON values.

use jsonnet_expr::{Number, Prim, Str};
use std::{collections::BTreeMap, fmt};

/// A JSON value.
#[derive(Debug, PartialEq, Eq)]
pub(crate) enum Val {
  Prim(Prim),
  Object(BTreeMap<Str, Val>),
  Array(Vec<Val>),
}

impl Val {
  pub(crate) fn from_serde(ar: &jsonnet_expr::StrArena, serde: serde_json::Value) -> Self {
    match serde {
      serde_json::Value::Null => Self::Prim(Prim::Null),
      serde_json::Value::Bool(b) => Self::Prim(Prim::Bool(b)),
      serde_json::Value::Number(num) => {
        let num = num.as_f64().unwrap();
        let num = Number::try_from(num).unwrap();
        Self::Prim(Prim::Number(num))
      }
      serde_json::Value::String(str) => {
        let str = ar.str_shared(str.into_boxed_str());
        Self::Prim(Prim::String(str))
      }
      serde_json::Value::Array(vs) => {
        let iter = vs.into_iter().map(|v| Self::from_serde(ar, v));
        Self::Array(iter.collect())
      }
      serde_json::Value::Object(map) => {
        let iter = map.into_iter().map(|(k, v)| {
          let k = ar.str_shared(k.into_boxed_str());
          let v = Self::from_serde(ar, v);
          (k, v)
        });
        Self::Object(iter.collect())
      }
    }
  }

  #[must_use]
  pub(crate) fn display<'a>(&'a self, ar: &'a jsonnet_expr::StrArena) -> impl fmt::Display + 'a {
    DisplayVal { val: self, ar }
  }
}

struct DisplayVal<'a> {
  val: &'a Val,
  ar: &'a jsonnet_expr::StrArena,
}

impl fmt::Display for DisplayVal<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self.val {
      Val::Prim(p) => p.display(self.ar).fmt(f),
      Val::Object(map) => {
        f.write_str("{")?;
        let mut iter = map.iter();
        let mut empty = true;
        if let Some((k, v)) = iter.next() {
          // TODO handle escapes
          f.write_str(" \"")?;
          self.ar.get(k).fmt(f)?;
          f.write_str("\": ")?;
          v.display(self.ar).fmt(f)?;
          empty = false;
        }
        for (k, v) in iter {
          // TODO handle escapes
          f.write_str(", \"")?;
          self.ar.get(k).fmt(f)?;
          f.write_str("\": ")?;
          v.display(self.ar).fmt(f)?;
          empty = false;
        }
        if !empty {
          f.write_str(" ")?;
        }
        f.write_str("}")
      }
      Val::Array(vs) => {
        f.write_str("[")?;
        let mut iter = vs.iter();
        if let Some(v) = iter.next() {
          v.display(self.ar).fmt(f)?;
        }
        for v in iter {
          f.write_str(", ")?;
          v.display(self.ar).fmt(f)?;
        }
        f.write_str("]")
      }
    }
  }
}
