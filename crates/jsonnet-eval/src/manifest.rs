//! Manifesting Jsonnet values into JSON values.
//!
//! Since Jsonnet values are lazy, they can contain unexecuted Jsonnet expressions. Manifestation is
//! thus mutually recursive with execution.

use crate::{exec, val};
use jsonnet_expr::Prim;
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
  Function,
  Exec(exec::Error),
}

pub type Result<T = Val> = std::result::Result<T, Error>;

/// # Errors
///
/// If manifestation failed.
pub fn manifest(val: val::Val) -> Result {
  match val {
    val::Val::Prim(prim) => Ok(Val::Prim(prim)),
    val::Val::Rec { env: _, kind } => match kind {
      val::RecValKind::Object { .. } => todo!(),
      val::RecValKind::Function { .. } => Err(Error::Function),
      val::RecValKind::Array(_) => todo!(),
    },
  }
}
