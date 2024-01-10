use std::path::Path;

#[derive(Clone, Copy)]
pub(crate) struct Cx<'a> {
  pub(crate) current_dir: &'a Path,
  pub(crate) other_dirs: &'a [&'a Path],
  pub(crate) fs: &'a dyn FileSystem,
}

impl<'a> Cx<'a> {
  pub(crate) fn dirs(&self) -> impl Iterator<Item = &'a Path> {
    std::iter::once(self.current_dir).chain(self.other_dirs.iter().copied())
  }
}

/// The filesystem operations we need.
pub trait FileSystem {
  /// Returns a canonical path buf for the given path.
  ///
  /// # Errors
  ///
  /// If the filesystem failed
  fn canonicalize(&self, p: &Path) -> std::io::Result<paths::CanonicalPathBuf>;
}
