use paths::{CanonicalPath, CanonicalPathBuf};

#[derive(Clone, Copy)]
pub(crate) struct Cx<'a> {
  pub(crate) current_dir: &'a CanonicalPath,
  pub(crate) other_dirs: &'a [CanonicalPathBuf],
  pub(crate) fs: &'a dyn FileSystem,
}

impl<'a> Cx<'a> {
  pub(crate) fn dirs(&self) -> impl Iterator<Item = &'a CanonicalPath> {
    std::iter::once(self.current_dir)
      .chain(self.other_dirs.iter().map(CanonicalPathBuf::as_canonical_path))
  }
}

/// The filesystem operations we need.
pub trait FileSystem {
  /// Makes a path canonical.
  /// # Errors
  /// If the filesystem failed.
  fn canonical(&self, p: &std::path::Path) -> std::io::Result<paths::CanonicalPathBuf>;
}
