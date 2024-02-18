use paths::CanonicalPath;

#[derive(Clone, Copy)]
pub(crate) struct Cx<'a> {
  pub(crate) current_dir: &'a CanonicalPath,
  pub(crate) other_dirs: &'a [&'a CanonicalPath],
  pub(crate) fs: &'a dyn FileSystem,
}

impl<'a> Cx<'a> {
  pub(crate) fn dirs(&self) -> impl Iterator<Item = &'a CanonicalPath> {
    std::iter::once(self.current_dir).chain(self.other_dirs.iter().copied())
  }
}

/// The filesystem operations we need.
pub trait FileSystem {
  /// Makes a path canonical.
  /// # Errors
  /// If the filesystem failed.
  fn canonical(&self, p: &std::path::Path) -> std::io::Result<paths::CanonicalPathBuf>;
}
