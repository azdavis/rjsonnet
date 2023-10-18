//! Manifesting Jsonnet values into JSON values.

#![deny(clippy::pedantic, missing_debug_implementations, rust_2018_idioms)]

use jsonnet_prim::Prim;
use rustc_hash::FxHashMap;

#[derive(Debug)]
pub enum Val {
  Prim(Prim),
  Object(FxHashMap<String, Val>),
  Array(Vec<Val>),
}

#[derive(Debug)]
pub enum Error {
  Function,
}

pub type Manifest<T = Val> = Result<T, Error>;

/// # Errors
///
/// If manifestation failed.
pub fn get(val: jsonnet_val::Val) -> Manifest {
  match val {
    jsonnet_val::Val::Prim(prim) => Ok(Val::Prim(prim)),
    jsonnet_val::Val::Rec { env: _, kind } => match kind {
      jsonnet_val::RecValKind::Object { .. } => todo!(),
      jsonnet_val::RecValKind::Function { .. } => Err(Error::Function),
      jsonnet_val::RecValKind::Array(_) => todo!(),
    },
  }
}
