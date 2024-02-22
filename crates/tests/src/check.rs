use paths::FileSystem;
use rustc_hash::{FxHashMap, FxHashSet};
use std::path::{Path, PathBuf};

const DEFAULT_PATH: &str = "/f.jsonnet";

#[derive(Default)]
struct Input<'a> {
  jsonnet: FxHashMap<&'a str, JsonnetInput<'a>>,
  raw: FxHashMap<&'a str, &'a str>,
  add: FxHashSet<&'a str>,
}

impl<'a> Input<'a> {
  fn with_jsonnet(mut self, path: &'a str, jsonnet: JsonnetInput<'a>) -> Self {
    assert!(self.jsonnet.insert(path, jsonnet).is_none());
    self
  }

  // TODO use
  #[allow(dead_code)]
  fn with_raw(mut self, path: &'a str, contents: &'a str) -> Self {
    assert!(self.raw.insert(path, contents).is_none());
    self
  }

  fn add(mut self, path: &'a str) -> Self {
    self.add.insert(path);
    self
  }

  fn add_all(mut self) -> Self {
    self.add.extend(self.jsonnet.keys().chain(self.raw.keys()).copied());
    self
  }

  fn check(self) {
    _ = env_logger::builder().is_test(true).filter_level(log::LevelFilter::Debug).try_init();

    let files = std::iter::empty()
      .chain(self.jsonnet.iter().map(|(&path, jsonnet)| (path, jsonnet.text)))
      .chain(self.raw.iter().map(|(&path, &text)| (path, text)))
      .map(|(path, text)| (PathBuf::from(path), text.to_owned()));
    let fs = paths::MemoryFileSystem::new(files.collect());

    let add: Vec<_> =
      self.add.iter().map(|&path| fs.canonical(Path::new(path)).expect("canonical")).collect();

    let mut st = jsonnet_analyze::St::default();

    for (path, ds) in st.update_many(&fs, Vec::new(), add) {
      if let Some(d) = ds.first() {
        let path = st.paths().get_path(path).as_path();
        panic!("{} at {}: diagnostic: {}", path.display(), d.range, d.message);
      }
    }

    for (&path, jsonnet) in &self.jsonnet {
      let path = fs.canonical(Path::new(path)).expect("canonical");
      let path = st.path_id(path);

      match (jsonnet.kind, st.get_json(path)) {
        (OutcomeKind::Manifest, Ok(got)) => {
          let want: serde_json::Value =
            serde_json::from_str(jsonnet.outcome).expect("test input json");
          let want = jsonnet_eval::Json::from_serde(st.strings(), want);
          if want != *got {
            let want = want.display(st.strings());
            let got = got.display(st.strings());
            panic!("want: {want}\ngot:  {got}");
          }
        }

        (OutcomeKind::String, Ok(got)) => got.assert_is_str(st.strings(), jsonnet.outcome),

        (OutcomeKind::Error, Err(err)) => {
          let got = err.display(st.strings(), st.paths()).to_string();
          assert_eq!(jsonnet.outcome, got.as_str());
        }

        (OutcomeKind::Error, Ok(got)) => panic!("no error, got json: {got:?}"),
        (OutcomeKind::Manifest | OutcomeKind::String, Err(err)) => {
          let got = err.display(st.strings(), st.paths()).to_string();
          panic!("error: {got:?}");
        }
      }
    }
  }
}

struct JsonnetInput<'a> {
  text: &'a str,
  outcome: &'a str,
  kind: OutcomeKind,
}

impl<'a> JsonnetInput<'a> {
  fn manifest(text: &'a str, json: &'a str) -> Self {
    Self { text, outcome: json, kind: OutcomeKind::Manifest }
  }

  fn string(text: &'a str, string: &'a str) -> Self {
    Self { text, outcome: string, kind: OutcomeKind::String }
  }

  fn error(text: &'a str, message: &'a str) -> Self {
    Self { text, outcome: message, kind: OutcomeKind::Error }
  }
}

#[derive(Debug, Clone, Copy)]
enum OutcomeKind {
  Manifest,
  String,
  Error,
}

/// tests that for each triple of (path, jsonnet, json), each jsonnet manifests to its json.
pub(crate) fn manifest_many(files: &[(&str, &str, &str)]) {
  let mut inp = Input::default();
  for &(path, jsonnet, json) in files {
    inp = inp.with_jsonnet(path, JsonnetInput::manifest(jsonnet, json));
  }
  inp.add_all().check();
}

/// tests that for each triple of (path, jsonnet, json), each jsonnet manifests to its json, when
/// adding the given paths.
///
/// this is similar to `manifest_many` but helps test the auto-discovery mechanism. this is where if
/// you ask to add some files but those files import files that you didn't ask to add, the impl
/// should discover and add them for you.
pub(crate) fn manifest_many_add(files: &[(&str, &str, &str)], add: &[&str]) {
  let mut inp = Input::default();
  for &(path, jsonnet, json) in files {
    inp = inp.with_jsonnet(path, JsonnetInput::manifest(jsonnet, json));
  }
  for &path in add {
    inp = inp.add(path);
  }
  inp.check();
}

fn one(jsonnet: JsonnetInput<'_>) {
  Input::default().with_jsonnet(DEFAULT_PATH, jsonnet).add_all().check();
}

/// tests that `jsonnet` manifests to the `json`.
pub(crate) fn manifest(jsonnet: &str, json: &str) {
  one(JsonnetInput::manifest(jsonnet, json));
}

/// tests that `s`, when treated as either jsonnet or json, manifests to the same thing.
pub(crate) fn manifest_self(s: &str) {
  manifest(s, s);
}

/// tests that `jsonnet` manifests to the string `want`.
///
/// NOTE: `want` is NOT interpreted as JSON.
pub(crate) fn manifest_str(jsonnet: &str, want: &str) {
  one(JsonnetInput::string(jsonnet, want));
}

/// tests that `jsonnet` execution results in an error whose message is `want`.
pub(crate) fn exec_err(jsonnet: &str, want: &str) {
  one(JsonnetInput::error(jsonnet, want));
}
