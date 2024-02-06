use anyhow::Result;
use std::path::PathBuf;

pub trait State {
  const ALL_EXTS: &'static str;
  fn is_ext(&self, s: &str) -> bool;
  fn update_many<F>(&mut self, fs: &F, remove: Vec<PathBuf>, add: Vec<PathBuf>)
  where
    F: Sync + Send + paths::FileSystem;
  fn hover(&mut self, path: paths::CanonicalPathBuf) -> Result<String>;
}
