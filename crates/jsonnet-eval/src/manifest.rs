//! Manifesting Jsonnet values into JSON values.
//!
//! Since Jsonnet values are lazy, they can contain unexecuted Jsonnet expressions. Manifestation is
//! thus mutually recursive with execution.

use crate::{exec, val};
use jsonnet_expr::{Arenas, Prim};
use rustc_hash::FxHashMap;

/// A JSON value.
#[derive(Debug)]
pub enum Val {
  Prim(Prim),
  Object(FxHashMap<String, Val>),
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

/// # Errors
///
/// If manifestation failed.
pub fn get(ars: &Arenas, val: val::Val) -> Result {
  match val {
    val::Val::Prim(prim) => Ok(Val::Prim(prim)),
    val::Val::Rec { env, kind } => match kind {
      val::RecValKind::Object { .. } => todo!(),
      val::RecValKind::Function { .. } => Err(Error::Function),
      val::RecValKind::Array(exprs) => {
        let iter = exprs.into_iter().map(|expr| {
          let val = exec::get(&env, ars, expr)?;
          get(ars, val)
        });
        let vs = iter.collect::<Result<Vec<_>>>()?;
        Ok(Val::Array(vs))
      }
    },
  }
}
