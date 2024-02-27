//! A trait that describes what the "brain" of a language server must implement.
//!
//! This "brain" is not concerned with low-level LSP details like message formatting.

/// The state of a language server.
pub trait State {
  /// Make a new state with init options.
  fn new<F>(fs: &F, val: Option<serde_json::Value>) -> Self
  where
    F: paths::FileSystem;

  /// What to say to ask for a bug report.
  const BUG_REPORT_MSG: &'static str;

  /// A glob for all files that should be considered.
  const GLOB: &'static str;

  /// Whether this file extension should be considered.
  fn is_ext(&self, s: &str) -> bool;

  /// Update many files at once.
  #[must_use]
  fn update_many<F>(
    &mut self,
    fs: &F,
    remove: Vec<paths::CanonicalPathBuf>,
    add: Vec<paths::CanonicalPathBuf>,
  ) -> paths::PathMap<Vec<diagnostic::Diagnostic>>
  where
    F: Sync + Send + paths::FileSystem;

  /// Updates one file.
  #[must_use]
  fn update_one<F>(
    &mut self,
    fs: &F,
    path: paths::CanonicalPathBuf,
    contents: &str,
  ) -> paths::PathMap<Vec<diagnostic::Diagnostic>>
  where
    F: Sync + Send + paths::FileSystem;

  /// Hover over a file.
  ///
  /// # Errors
  ///
  /// If we couldn't show more info about the hovered file.
  fn hover(&mut self, path: paths::CanonicalPathBuf) -> Option<String>;

  /// Returns the paths store for this.
  fn paths(&self) -> &paths::Store;

  /// Returns a path id for this path.
  fn path_id(&mut self, path: paths::CanonicalPathBuf) -> paths::PathId;
}
