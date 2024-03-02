//! Testing infra.

mod expect;

use paths::FileSystem;
use rustc_hash::{FxHashMap, FxHashSet};
use std::path::{Path, PathBuf};

const DEFAULT_PATH: &str = "/f.jsonnet";

#[must_use]
#[derive(Default)]
pub(crate) struct Input<'a> {
  jsonnet: FxHashMap<&'a str, JsonnetInput<'a>>,
  raw: FxHashMap<&'a str, &'a str>,
  to_add: FxHashSet<&'a str>,
}

impl<'a> Input<'a> {
  pub(crate) fn with_jsonnet(mut self, path: &'a str, jsonnet: JsonnetInput<'a>) -> Self {
    assert!(self.jsonnet.insert(path, jsonnet).is_none());
    self
  }

  pub(crate) fn with_raw(mut self, path: &'a str, contents: &'a str) -> Self {
    assert!(self.raw.insert(path, contents).is_none());
    self
  }

  pub(crate) fn add(mut self, path: &'a str) -> Self {
    assert!(self.to_add.insert(path));
    self
  }

  pub(crate) fn add_all(mut self) -> Self {
    self.to_add.extend(self.jsonnet.keys().chain(self.raw.keys()).copied());
    self
  }

  pub(crate) fn check(self) {
    _ = env_logger::builder().is_test(true).filter_level(log::LevelFilter::Debug).try_init();

    let files = std::iter::empty()
      .chain(self.jsonnet.iter().map(|(&path, jsonnet)| (path, jsonnet.text)))
      .chain(self.raw.iter().map(|(&path, &text)| (path, text)))
      .map(|(path, text)| (PathBuf::from(path), text.to_owned()));
    let fs = paths::MemoryFileSystem::new(files.collect());

    assert!(!self.to_add.is_empty(), "must call .add() or .add_all() on the Input");
    let to_add: Vec<_> =
      self.to_add.iter().map(|&path| fs.canonical(Path::new(path)).expect("canonical")).collect();

    let init = jsonnet_analyze::Init {
      manifest: true,
      show_diagnostics: jsonnet_analyze::ShowDiagnostics::All,
      ..Default::default()
    };
    let mut st = jsonnet_analyze::St::new(&fs, init);

    for (path, ds) in st.update_many(&fs, Vec::new(), to_add) {
      if let Some(d) = ds.first() {
        let path = st.paths().get_path(path).as_path();
        panic!("{} at {}: diagnostic: {}", path.display(), d.range, d.message);
      }
    }

    let expects = self.jsonnet.iter().map(|(&path, jsonnet)| {
      let path = fs.canonical(Path::new(path)).expect("canonical");
      let path = st.path_id(path);
      let ex_file = expect::File::new(jsonnet.text);
      (path, ex_file)
    });
    let expects: paths::PathMap<_> = expects.collect();

    for (&path, jsonnet) in &self.jsonnet {
      let path = fs.canonical(Path::new(path)).expect("canonical");
      let path = st.path_id(path);

      let ex_file = &expects[&path];
      for (region, ex) in ex_file.iter() {
        match ex.kind {
          expect::Kind::Def => {}
          expect::Kind::Use => {
            let pos = text_pos::PositionUtf16 { line: region.line, col: region.col_start };
            let (def_path, range) = st.get_def(path, pos).expect("no def");
            assert_eq!(range.start.line, range.end.line);
            let region = expect::Region {
              line: range.start.line,
              col_start: range.start.col,
              col_end: range.end.col,
            };
            let def_ex = expects[&def_path].get(region).expect("nothing at def site");
            assert!(matches!(def_ex.kind, expect::Kind::Def));
            assert_eq!(def_ex.msg, ex.msg);
          }
        }
      }

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

#[must_use]
pub(crate) struct JsonnetInput<'a> {
  text: &'a str,
  outcome: &'a str,
  kind: OutcomeKind,
}

impl<'a> JsonnetInput<'a> {
  pub(crate) fn manifest(text: &'a str, json: &'a str) -> Self {
    Self { text, outcome: json, kind: OutcomeKind::Manifest }
  }

  pub(crate) fn manifest_self(text: &'a str) -> Self {
    Self { text, outcome: text, kind: OutcomeKind::Manifest }
  }

  pub(crate) fn string(text: &'a str, string: &'a str) -> Self {
    Self { text, outcome: string, kind: OutcomeKind::String }
  }

  pub(crate) fn error(text: &'a str, message: &'a str) -> Self {
    Self { text, outcome: message, kind: OutcomeKind::Error }
  }

  pub(crate) fn check_one(self) {
    Input::default().with_jsonnet(DEFAULT_PATH, self).add_all().check();
  }
}

#[derive(Debug, Clone, Copy)]
enum OutcomeKind {
  Manifest,
  String,
  Error,
}
