//! Resolve a path in some directories.

use paths::{CleanPath, CleanPathBuf};

/// Resolve the path to an import in the dirs, if possible.
pub fn get<'a, I, F>(path: &std::path::Path, mut dirs: I, fs: &F) -> Option<CleanPathBuf>
where
  I: Iterator<Item = &'a CleanPath>,
  F: ?Sized + FileSystem,
{
  dirs.find_map(|dir| {
    let path = dir.join(path);
    fs.is_file(path.as_path()).then_some(path)
  })
}

/// The filesystem operations we need.
pub trait FileSystem {
  /// Returns whether a path is a file that exists and which we can access.
  fn is_file(&self, p: &std::path::Path) -> bool;
}

// impl<'a> FileSystem for &'a dyn FileSystem {
//   fn is_file(&self, p: &std::path::Path) -> bool {
//     (*self).is_file(p)
//   }
// }

/// At least one dir.
#[derive(Debug, Clone, Copy)]
pub struct NonEmptyDirs<'a> {
  first: &'a CleanPath,
  other: &'a [CleanPathBuf],
}

impl<'a> NonEmptyDirs<'a> {
  /// Make a new one of these.
  #[must_use]
  pub fn new(first: &'a CleanPath, other: &'a [CleanPathBuf]) -> Self {
    Self { first, other }
  }

  /// Return all the dirs, first the first one, then the others in order.
  pub fn iter(&self) -> impl Iterator<Item = &'a CleanPath> {
    std::iter::once(self.first).chain(self.other.iter().map(CleanPathBuf::as_clean_path))
  }
}
