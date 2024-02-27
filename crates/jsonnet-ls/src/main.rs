//! A language server for Jsonnet.

use always::BUG_REPORT_MSG;

fn main() {
  let mut st = State(jsonnet_analyze::St::new(false, Vec::new()));
  lang_srv::run(&mut st);
}

struct State(jsonnet_analyze::St);

impl lang_srv::State for State {
  fn crash_msg(&self) -> String {
    BUG_REPORT_MSG.to_owned()
  }

  const GLOB: &'static str = "**/*.{jsonnet,libsonnet,TEMPLATE}";

  fn is_ext(&self, s: &str) -> bool {
    matches!(s, "jsonnet" | "libsonnet" | "TEMPLATE")
  }

  fn update_many<F>(
    &mut self,
    fs: &F,
    remove: Vec<paths::CanonicalPathBuf>,
    add: Vec<paths::CanonicalPathBuf>,
  ) -> paths::PathMap<Vec<diagnostic::Diagnostic>>
  where
    F: Sync + Send + paths::FileSystem,
  {
    self.0.update_many(fs, remove, add)
  }

  fn update_one<F>(
    &mut self,
    fs: &F,
    path: paths::CanonicalPathBuf,
    contents: &str,
  ) -> paths::PathMap<Vec<diagnostic::Diagnostic>>
  where
    F: Sync + Send + paths::FileSystem,
  {
    self.0.update_one(fs, path.as_canonical_path(), contents)
  }

  /// TODO take a text range thing
  fn hover(&mut self, path: paths::CanonicalPathBuf) -> Option<String> {
    let path_id = self.0.path_id(path);
    let json = match self.0.get_json(path_id) {
      Ok(x) => x,
      Err(e) => {
        log::error!("couldn't get json: {}", e.display(self.0.strings(), self.paths()));
        return None;
      }
    };
    Some(json.display(self.0.strings()).to_string())
  }

  fn paths(&self) -> &paths::Store {
    self.0.paths()
  }

  fn path_id(&mut self, path: paths::CanonicalPathBuf) -> paths::PathId {
    self.0.path_id(path)
  }
}
