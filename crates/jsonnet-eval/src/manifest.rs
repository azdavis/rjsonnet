use crate::val::{RecValKind, Val};
use crate::{Error, Eval};
use jsonnet_expr::Prim;
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
pub fn manifest(val: Val) -> Eval<JsonVal> {
  match val {
    Val::Prim(prim) => Ok(JsonVal::Prim(prim)),
    Val::Rec { env: _, kind } => match kind {
      RecValKind::Object { .. } => todo!(),
      RecValKind::Function { .. } => Err(Error::Function),
      RecValKind::Array(_) => todo!(),
    },
  }
}
