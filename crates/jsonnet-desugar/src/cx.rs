//! The immutable context under which we desugar.

use jsonnet_resolve_import::{FileSystem, NonEmptyDirs};

#[derive(Clone, Copy)]
pub(crate) struct Cx<'a> {
  pub(crate) dirs: NonEmptyDirs<'a>,
  pub(crate) fs: &'a dyn FileSystem,
}
