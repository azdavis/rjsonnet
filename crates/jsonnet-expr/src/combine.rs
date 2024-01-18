use crate::{Artifacts, ExprArena, ExprData, Prim, StrIdx, StrRepr};
use rustc_hash::FxHashMap;

/// Combine.
///
/// # Panics
///
/// Upon internal error.
pub fn get(art: &mut Artifacts, other: Artifacts, ar: &mut ExprArena) {
  let mut strings = FxHashMap::<StrIdx, StrIdx>::default();
  art.strings.combine(other.strings, &mut |old, new| {
    assert!(strings.insert(old, new).is_none());
  });
  for (_, expr) in ar.iter_mut() {
    match expr {
      ExprData::Prim(prim) => match prim {
        Prim::String(s) => match &mut s.0 {
          StrRepr::Idx(idx) => *idx = strings[idx],
          StrRepr::Alloc(_) => {}
        },
        Prim::Null | Prim::Bool(_) | Prim::Number(_) => {}
      },
      ExprData::ObjectComp { id, .. } | ExprData::Id(id) => id.0 = strings[&id.0],
      ExprData::Local { binds, .. } | ExprData::Call { named: binds, .. } => {
        for (id, _) in binds {
          id.0 = strings[&id.0];
        }
      }
      ExprData::Function { params, .. } => {
        for (id, _) in params {
          id.0 = strings[&id.0];
        }
      }
      ExprData::Import { .. } => todo!("combine Import"),
      ExprData::Object { .. }
      | ExprData::Array(_)
      | ExprData::Subscript { .. }
      | ExprData::If { .. }
      | ExprData::BinaryOp { .. }
      | ExprData::UnaryOp { .. }
      | ExprData::Error(_) => {}
    }
  }
}
