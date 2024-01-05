//! JSON values.

use jsonnet_expr::{Prim, Str};
use std::{collections::BTreeMap, fmt};

/// A JSON value.
#[derive(Debug, PartialEq, Eq)]
pub enum Val {
  Prim(Prim),
  Object(BTreeMap<Str, Val>),
  Array(Vec<Val>),
}

impl Val {
  #[must_use]
  pub fn display<'a>(&'a self, ar: &'a jsonnet_expr::StrArena) -> impl fmt::Display + 'a {
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
