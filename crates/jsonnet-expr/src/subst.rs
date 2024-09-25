//! See [`Subst`].

use crate::{Artifacts, NotBuiltinStr, StrIdx};
use always::always;
use rustc_hash::FxHashMap;

/// A substitution, from combining artifacts.
#[derive(Debug, Default)]
pub struct Subst {
  strings: FxHashMap<StrIdx, StrIdx>,
  paths: paths::PathMap<paths::PathId>,
}

impl Subst {
  /// Combine artifacts and produce a substitution to apply to other things.
  pub fn get(this: &mut Artifacts, other: Artifacts) -> Self {
    let mut ret = Subst::default();
    for (idx, s) in other.strings.idx_to_data.into_iter().enumerate() {
      let old = StrIdx::from_usize(idx);
      let new = this.strings.dangerous_mk_idx(s, NotBuiltinStr::from_str_arena());
      if old != new {
        always!(ret.strings.insert(old, new).is_none());
      }
    }
    this.paths.combine(other.paths, &mut |old, new| {
      if old != new {
        always!(ret.paths.insert(old, new).is_none());
      }
    });
    ret
  }

  /// Get the path id from the subst.
  #[must_use]
  pub fn get_path_id(&self, path: paths::PathId) -> paths::PathId {
    self.paths.get(&path).copied().unwrap_or(path)
  }

  /// Get the path id from the subst.
  #[must_use]
  pub(crate) fn get_str_idx(&self, idx: StrIdx) -> StrIdx {
    self.strings.get(&idx).copied().unwrap_or(idx)
  }
}
