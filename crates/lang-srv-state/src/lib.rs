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

  /// A glob for all paths that should be considered.
  const GLOB: &'static str;

  /// Whether this file path extension should be considered.
  fn is_ext(&self, s: &str) -> bool;

  /// Update many paths at once.
  #[must_use]
  fn update_many<F>(
    &mut self,
    fs: &F,
    remove: Vec<paths::CleanPathBuf>,
    add: Vec<paths::CleanPathBuf>,
  ) -> paths::PathMap<Vec<diagnostic::Diagnostic>>
  where
    F: Sync + Send + paths::FileSystem;

  /// Updates one path.
  #[must_use]
  fn update_one<F>(
    &mut self,
    fs: &F,
    path: paths::CleanPathBuf,
    changes: Vec<apply_changes::Change>,
  ) -> paths::PathMap<Vec<diagnostic::Diagnostic>>
  where
    F: Sync + Send + paths::FileSystem;

  /// Opens a path.
  #[must_use]
  fn open<F>(
    &mut self,
    fs: &F,
    path: paths::CleanPathBuf,
    contents: String,
  ) -> paths::PathMap<Vec<diagnostic::Diagnostic>>
  where
    F: Sync + Send + paths::FileSystem;

  /// Closes a path.
  #[must_use]
  fn close(&mut self, path: paths::CleanPathBuf) -> paths::PathMap<Vec<diagnostic::Diagnostic>>;

  /// Returns whether the path is open.
  #[must_use]
  fn is_open(&mut self, path: &paths::CleanPath) -> bool;

  /// Hover over a file.
  ///
  /// # Errors
  ///
  /// If we couldn't show more info about the hovered file.
  fn hover<F>(&mut self, fs: &F, path: paths::CleanPathBuf) -> Option<String>
  where
    F: Sync + Send + paths::FileSystem;

  /// Get the definition site of a part of a file.
  fn get_def<F>(
    &mut self,
    fs: &F,
    path: paths::CleanPathBuf,
    pos: text_pos::PositionUtf16,
  ) -> Option<(paths::PathId, text_pos::RangeUtf16)>
  where
    F: Sync + Send + paths::FileSystem;

  /// Returns the paths store for this.
  fn paths(&self) -> &paths::Store;

  /// Returns a path id for this path.
  fn path_id(&mut self, path: paths::CleanPathBuf) -> paths::PathId;
}
