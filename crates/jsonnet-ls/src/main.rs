//! A language server for Jsonnet.

use always::always;
use serde_json::Value;

fn main() {
  lang_srv::run::<State>();
}

struct State(jsonnet_analyze::St);

impl lang_srv::State for State {
  fn new<F>(fs: &F, val: Option<Value>) -> Self
  where
    F: paths::FileSystem,
  {
    let mut init = jsonnet_analyze::Init::default();
    let pwd = fs.current_dir();
    let mut logger_env = env_logger::Env::default();
    // TODO is it correct to use pwd here or should we be using the root dir from the language
    // server init object (which is NOT the `val` init object passed to us here)?
    if let Ok(pwd) = &pwd {
      init.relative_to = Some(pwd.to_owned());
    }
    // TODO report errors from bad init object
    if let Some(Value::Object(obj)) = val {
      if let Some(filter) = obj.get("logger_filter").and_then(Value::as_str) {
        if !filter.is_empty() {
          logger_env = logger_env.default_filter_or(filter.to_owned());
        }
      }

      init.manifest = obj.get("manifest").and_then(Value::as_bool).unwrap_or_default();
      init.root_dirs = obj
        .get("root_dirs")
        .and_then(|x| {
          let ary = x.as_array()?;
          let pwd = pwd.ok()?;
          ary
            .iter()
            .map(|x| x.as_str().map(|x| pwd.as_clean_path().join(x)))
            .collect::<Option<Vec<_>>>()
        })
        .unwrap_or_default();
      init.show_diagnostics = obj
        .get("show_diagnostics")
        .and_then(|x| x.as_str()?.parse::<jsonnet_analyze::ShowDiagnostics>().ok())
        .unwrap_or_default();
    }

    if let Err(e) = env_logger::try_init_from_env(logger_env) {
      always!(false, "couldn't init logger: {e}");
    }

    Self(jsonnet_analyze::St::new(init))
  }

  const BUG_REPORT_MSG: &'static str = always::BUG_REPORT_MSG;

  const GLOB: &'static str = "**/*.{jsonnet,libsonnet,TEMPLATE}";

  fn is_ext(&self, s: &str) -> bool {
    matches!(s, "jsonnet" | "libsonnet" | "TEMPLATE")
  }

  fn update_many<F>(
    &mut self,
    fs: &F,
    remove: Vec<paths::CleanPathBuf>,
    add: Vec<paths::CleanPathBuf>,
  ) -> paths::PathMap<Vec<diagnostic::Diagnostic>>
  where
    F: Sync + Send + paths::FileSystem,
  {
    self.0.update_many(fs, remove, add)
  }

  fn update_one<F>(
    &mut self,
    fs: &F,
    path: paths::CleanPathBuf,
    contents: &str,
  ) -> paths::PathMap<Vec<diagnostic::Diagnostic>>
  where
    F: Sync + Send + paths::FileSystem,
  {
    self.0.update_one(fs, path.as_clean_path(), contents)
  }

  /// TODO take a text range thing
  fn hover(&mut self, path: paths::CleanPathBuf) -> Option<String> {
    let path_id = self.0.path_id(path);
    let json = match self.0.get_json(path_id) {
      Ok(x) => x,
      Err(e) => {
        // NOTE we don't have the relative_to here
        log::error!("couldn't get json: {}", e.display(self.0.strings(), self.paths(), None));
        return None;
      }
    };
    Some(json.display(self.0.strings()).to_string())
  }

  fn get_def(
    &mut self,
    path: paths::CleanPathBuf,
    pos: text_pos::PositionUtf16,
  ) -> Option<(paths::PathId, text_pos::RangeUtf16)> {
    let path_id = self.0.path_id(path);
    self.0.get_def(path_id, pos)
  }

  fn paths(&self) -> &paths::Store {
    self.0.paths()
  }

  fn path_id(&mut self, path: paths::CleanPathBuf) -> paths::PathId {
    self.0.path_id(path)
  }
}
