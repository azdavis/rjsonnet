//! Manifesting Jsonnet values into JSON values.
//!
//! Since Jsonnet values are lazy, they can contain unexecuted Jsonnet expressions. Manifestation is
//! thus mutually recursive with execution.

use crate::{exec, val};
use jsonnet_expr::{Arenas, Prim, Str};
use rustc_hash::FxHashMap;

/// A JSON value.
#[derive(Debug)]
pub enum Val {
  Prim(Prim),
  Object(FxHashMap<Str, Val>),
  Array(Vec<Val>),
}

#[derive(Debug)]
pub enum Error {
  Exec(exec::Error),
  Function,
}

impl From<exec::Error> for Error {
  fn from(value: exec::Error) -> Self {
    Error::Exec(value)
  }
}

pub type Result<T = Val> = std::result::Result<T, Error>;

/// Manifests the Jsonnet value into a JSON value.
///
/// # Errors
///
/// If manifestation failed.
///
/// # Panics
///
/// Upon internal error.
pub fn get(ars: &Arenas, val: val::Val) -> Result {
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
      val::RecValKind::Function { .. } => Err(Error::Function),
      val::RecValKind::Array(exprs) => {
        let iter = exprs.into_iter().map(|expr| get_(&env, ars, expr));
        let vs = iter.collect::<Result<Vec<_>>>()?;
        Ok(Val::Array(vs))
      }
    },
  }
}

/// both executes and manifests
fn get_(env: &val::Env, ars: &Arenas, expr: jsonnet_expr::Expr) -> Result {
  let val = exec::get(env, ars, expr)?;
  get(ars, val)
}
