use anyhow::Result;
use std::path::PathBuf;

/// The state of a language server.
pub trait State {
  /// What to say when the server crashed.
  fn crash_msg(&self) -> String;

  /// A glob for all files that should be considered.
  const GLOB: &'static str;

  /// Whether this file extension should be considered.
  fn is_ext(&self, s: &str) -> bool;

  /// Update many files at once.
  fn update_many<F>(
    &mut self,
    fs: &F,
    remove: Vec<PathBuf>,
    add: Vec<PathBuf>,
  ) -> paths::PathMap<Vec<diagnostic::Diagnostic>>
  where
    F: Sync + Send + paths::FileSystem;

  /// Hover over a part of a file.
  ///
  /// # Errors
  ///
  /// If nothing was there to hover.
  fn hover(&mut self, path: paths::CanonicalPathBuf) -> Result<String>;

  /// Returns the paths store for this.
  fn paths(&mut self) -> &paths::Store;
}
