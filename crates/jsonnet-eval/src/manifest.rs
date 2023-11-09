//! Manifesting Jsonnet values into JSON values.
//!
//! Since Jsonnet values are lazy, they can contain unexecuted Jsonnet expressions. Manifestation is
//! thus mutually recursive with execution.

use crate::{error, exec, val};
use jsonnet_expr::{Arenas, Prim, Str};
use rustc_hash::FxHashMap;
use std::fmt;

/// A JSON value.
#[derive(Debug, PartialEq, Eq)]
pub enum Val {
  Prim(Prim),
  Object(FxHashMap<Str, Val>),
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

impl<'a> fmt::Display for DisplayVal<'a> {
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
          self.ar.get(*k).fmt(f)?;
          f.write_str("\": ")?;
          v.display(self.ar).fmt(f)?;
          empty = false;
        }
        for (k, v) in iter {
          // TODO handle escapes
          f.write_str(", \"")?;
          self.ar.get(*k).fmt(f)?;
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

/// Manifests the Jsonnet value into a JSON value.
///
/// # Errors
///
/// If manifestation failed.
///
/// # Panics
///
/// Upon internal error.
pub fn get(ars: &Arenas, val: val::Val) -> error::Result<Val> {
  match val {
    val::Val::Prim(prim) => Ok(Val::Prim(prim)),
    val::Val::Rec { env, kind } => match kind {
      val::RecValKind::Object { asserts, fields } => {
        for expr in asserts {
          get_(&env, ars, expr)?;
        }
        let mut val_fields = FxHashMap::default();
        for (name, (vis, expr)) in fields {
          if matches!(vis, jsonnet_expr::Visibility::Hidden) {
            continue;
          }
          let val = get_(&env, ars, expr)?;
          assert!(val_fields.insert(name, val).is_none());
        }
        Ok(Val::Object(val_fields))
      }
      val::RecValKind::Function { .. } => Err(error::Error::ManifestFn),
      val::RecValKind::Array(exprs) => {
        let iter = exprs.into_iter().map(|expr| get_(&env, ars, expr));
        let vs = iter.collect::<error::Result<Vec<_>>>()?;
        Ok(Val::Array(vs))
      }
    },
    val::Val::Std(std_val) => match std_val {
      val::Std::Cmp | val::Std::Equals => Err(error::Error::ManifestFn),
    },
  }
}

/// both executes and manifests
fn get_(env: &val::Env, ars: &Arenas, expr: jsonnet_expr::Expr) -> error::Result<Val> {
  let val = exec::get(env, ars, expr)?;
  get(ars, val)
}
