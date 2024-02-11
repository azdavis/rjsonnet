//! A language server for Jsonnet.

use anyhow::{bail, Result};
use std::path::PathBuf;

fn main() {
  let mut st = State::default();
  lang_srv::run(&mut st);
}

const NAME: &str = "jsonnet-ls";
const VERSION: &str = env!("CARGO_PKG_VERSION");
const ISSUES_URL: &str = "https://github.com/azdavis/rjsonnet/issues";

#[derive(Default)]
struct State(jsonnet_analyze::St);

impl lang_srv::State for State {
  fn crash_msg(&self) -> String {
    format!("{NAME} ({VERSION}) crashed. We would appreciate a bug report: {ISSUES_URL}")
  }

  const GLOB: &'static str = "**/*.{jsonnet,libsonnet,TEMPLATE}";

  fn is_ext(&self, s: &str) -> bool {
    matches!(s, "jsonnet" | "libsonnet" | "TEMPLATE")
  }

  fn update_many<F>(
    &mut self,
    fs: &F,
    remove: Vec<PathBuf>,
    add: Vec<PathBuf>,
  ) -> paths::PathMap<Vec<diagnostic::Diagnostic>>
  where
    F: Sync + Send + paths::FileSystem,
  {
    self.0.update_many(fs, remove, add)
  }

  fn hover(&mut self, path: paths::CanonicalPathBuf) -> Result<String> {
    let path_id = self.0.path_id(path);
    let json = match self.0.get_json(path_id) {
      Ok(x) => x,
      Err(e) => {
        bail!("couldn't get json: {}", e.display(self.0.strings(), self.paths()))
      }
    };
    Ok(json.display(self.0.strings()).to_string())
  }

  fn paths(&self) -> &paths::Store {
    self.0.paths()
  }
}
