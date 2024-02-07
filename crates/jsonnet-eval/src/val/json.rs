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
        let num = num.as_f64().expect("convert json number to f64");
        let num = Number::try_from(num).expect("json cannot have NaN or inf");
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
  pub(crate) fn display<'a>(
    &'a self,
    ar: &'a jsonnet_expr::StrArena,
    indent: usize,
  ) -> impl fmt::Display + 'a {
    DisplayVal { val: self, ar, indent }
  }
}

struct DisplayVal<'a> {
  val: &'a Val,
  ar: &'a jsonnet_expr::StrArena,
  indent: usize,
}

impl fmt::Display for DisplayVal<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self.val {
      Val::Prim(p) => p.display(self.ar).fmt(f),
      Val::Object(map) => {
        f.write_str("{")?;
        let iter =
          map.iter().map(|(k, v)| DisplayKv { k, v, ar: self.ar, indent: self.indent + 1 });
        write_comma_sep(iter, self.indent, f)?;
        f.write_str("}")
      }
      Val::Array(vs) => {
        f.write_str("[")?;
        let iter = vs.iter().map(|v| v.display(self.ar, self.indent + 1));
        write_comma_sep(iter, self.indent, f)?;
        f.write_str("]")
      }
    }
  }
}

struct DisplayKv<'a> {
  k: &'a Str,
  v: &'a Val,
  ar: &'a jsonnet_expr::StrArena,
  indent: usize,
}

impl fmt::Display for DisplayKv<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    // TODO handle escapes
    f.write_str("\"")?;
    self.ar.get(self.k).fmt(f)?;
    f.write_str("\": ")?;
    self.v.display(self.ar, self.indent).fmt(f)
  }
}

fn write_comma_sep<I, T>(mut iter: I, indent: usize, f: &mut fmt::Formatter<'_>) -> fmt::Result
where
  I: Iterator<Item = T>,
  T: fmt::Display,
{
  let mut empty = true;
  if let Some(x) = iter.next() {
    f.write_str("\n")?;
    write_indent(indent + 1, f)?;
    x.fmt(f)?;
    empty = false;
  }
  for x in iter {
    f.write_str(",\n")?;
    write_indent(indent + 1, f)?;
    x.fmt(f)?;
    empty = false;
  }
  if !empty {
    f.write_str("\n")?;
    write_indent(indent, f)?;
  }
  Ok(())
}

fn write_indent(n: usize, f: &mut fmt::Formatter<'_>) -> fmt::Result {
  for _ in 0..n {
    f.write_str("  ")?;
  }
  Ok(())
}
