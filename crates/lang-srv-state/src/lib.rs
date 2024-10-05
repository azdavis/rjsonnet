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

  /// Mark many paths as updated.
  fn mark_as_updated(&mut self, updated: Vec<paths::CleanPathBuf>);

  /// Updates one path.
  fn update_one<F>(
    &mut self,
    fs: &F,
    path: paths::CleanPathBuf,
    changes: Vec<apply_changes::Change>,
  ) -> (paths::PathId, Vec<diagnostic::Diagnostic>)
  where
    F: Sync + paths::FileSystem;

  /// Opens a path.
  fn open<F>(
    &mut self,
    fs: &F,
    path: paths::CleanPathBuf,
    contents: String,
  ) -> (paths::PathId, Vec<diagnostic::Diagnostic>)
  where
    F: Sync + paths::FileSystem;

  /// Closes a path.
  fn close(&mut self, path: paths::CleanPathBuf) -> paths::PathMap<Vec<diagnostic::Diagnostic>>;

  /// Returns whether the path is open.
  fn is_open(&mut self, path: &paths::CleanPath) -> bool;

  /// Hover over part of a file.
  fn hover<F>(
    &mut self,
    fs: &F,
    path: paths::CleanPathBuf,
    pos: text_pos::PositionUtf16,
  ) -> Option<String>
  where
    F: Sync + paths::FileSystem;

  /// Get completions for part of a file.
  fn completions<F>(
    &mut self,
    fs: &F,
    path: paths::CleanPathBuf,
    pos: text_pos::PositionUtf16,
  ) -> Option<Vec<CompletionItem>>
  where
    F: Sync + paths::FileSystem;

  /// Get the definition site of a part of a file.
  fn get_def<F>(
    &mut self,
    fs: &F,
    path: paths::CleanPathBuf,
    pos: text_pos::PositionUtf16,
  ) -> Option<(paths::PathId, text_pos::RangeUtf16)>
  where
    F: Sync + paths::FileSystem;

  /// Returns the paths store for this.
  fn paths(&self) -> &paths::Store;

  /// Returns a path id for this path.
  fn path_id(&mut self, path: paths::CleanPathBuf) -> paths::PathId;
}

/// A completion item.
#[derive(Debug)]
pub struct CompletionItem {
  /// The name.
  pub name: String,
  /// The type.
  pub ty: String,
  /// The kind.
  pub kind: CompletionItemKind,
}

/// A kind of completion item.
#[derive(Debug, Clone, Copy)]
pub enum CompletionItemKind {
  /// A field, like for a "struct" or "object".
  Field,
}
