use paths::AbsPath;

#[derive(Clone, Copy)]
pub(crate) struct Cx<'a> {
  pub(crate) current_dir: &'a AbsPath,
  pub(crate) other_dirs: &'a [&'a AbsPath],
  pub(crate) fs: &'a dyn FileSystem,
}

impl<'a> Cx<'a> {
  pub(crate) fn dirs(&self) -> impl Iterator<Item = &'a AbsPath> {
    std::iter::once(self.current_dir).chain(self.other_dirs.iter().copied())
  }
}

/// The filesystem operations we need.
pub trait FileSystem {
  /// Returns true if this path is a file.
  fn is_file(&self, p: &std::path::Path) -> bool;
}
