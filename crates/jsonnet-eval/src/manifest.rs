use crate::{Error, Eval};
use jsonnet_prim::Prim;
use jsonnet_val::Val;
use rustc_hash::FxHashMap;

#[derive(Debug)]
pub enum JsonVal {
  Prim(Prim),
  Object(FxHashMap<String, Val>),
  Array(Vec<Val>),
}

/// # Errors
///
/// If manifestation failed.
pub fn manifest(val: jsonnet_val::Val) -> Eval<JsonVal> {
  match val {
    jsonnet_val::Val::Prim(prim) => Ok(JsonVal::Prim(prim)),
    jsonnet_val::Val::Rec { env: _, kind } => match kind {
      jsonnet_val::RecValKind::Object { .. } => todo!(),
      jsonnet_val::RecValKind::Function { .. } => Err(Error::Function),
      jsonnet_val::RecValKind::Array(_) => todo!(),
    },
  }
}
