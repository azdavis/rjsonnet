//! The immutable context under which we desugar.

use paths::{CleanPath, CleanPathBuf};

#[derive(Clone, Copy)]
pub(crate) struct Cx<'a> {
  pub(crate) current_dir: &'a CleanPath,
  pub(crate) other_dirs: &'a [CleanPathBuf],
  pub(crate) fs: &'a dyn FileSystem,
}

impl<'a> Cx<'a> {
  pub(crate) fn dirs(&self) -> impl Iterator<Item = &'a CleanPath> {
    std::iter::once(self.current_dir).chain(self.other_dirs.iter().map(CleanPathBuf::as_clean_path))
  }
}

/// The filesystem operations we need.
pub trait FileSystem {
  /// Returns whether a path is a file that exists and which we can access.
  fn is_file(&self, p: &std::path::Path) -> bool;
}
