use crate::{Artifacts, ExprArena};

/// Combine.
pub fn get(art: &mut Artifacts, other: Artifacts, _: &mut ExprArena) {
  // TODO
  *art = other;
}
