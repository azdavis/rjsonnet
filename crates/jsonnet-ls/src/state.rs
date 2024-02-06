use anyhow::{bail, Result};
use std::path::PathBuf;

pub trait State {
  fn update_many<F>(&mut self, fs: &F, remove: Vec<PathBuf>, add: Vec<PathBuf>)
  where
    F: Sync + Send + paths::FileSystem;
  fn hover(&mut self, path: paths::CanonicalPathBuf) -> Result<String>;
}

impl State for jsonnet_analyze::St {
  fn update_many<F>(&mut self, fs: &F, remove: Vec<PathBuf>, add: Vec<PathBuf>)
  where
    F: Sync + Send + paths::FileSystem,
  {
    jsonnet_analyze::St::update_many(self, fs, remove, add);
  }

  fn hover(&mut self, path: paths::CanonicalPathBuf) -> Result<String> {
    let path_id = self.path_id(path);
    let json = match self.get_json(path_id) {
      Ok(x) => x,
      Err(e) => bail!("couldn't get json: {}", e.display(self.strings())),
    };
    Ok(json.display(self.strings()).to_string())
  }
}
