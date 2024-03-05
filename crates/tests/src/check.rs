//! Testing infra.

mod expect;

use paths::FileSystem as _;
use rustc_hash::{FxHashMap, FxHashSet};

const DEFAULT_PATH: &str = "/f.jsonnet";

#[must_use]
#[derive(Default)]
pub(crate) struct MultiInput<'a> {
  inputs: Vec<Input<'a>>,
}

impl<'a> MultiInput<'a> {
  pub(crate) fn with_input(mut self, input: Input<'a>) -> Self {
    self.inputs.push(input);
    self
  }

  pub(crate) fn check(self) {
    _ = env_logger::builder().is_test(true).filter_level(log::LevelFilter::Debug).try_init();
    let mut fs = paths::MemoryFileSystem::default();
    let pwd = fs.current_dir().expect("no current dir for in-mem fs");
    let init = jsonnet_analyze::Init {
      relative_to: Some(pwd.clone()),
      manifest: true,
      show_diagnostics: jsonnet_analyze::ShowDiagnostics::All,
      ..Default::default()
    };
    let mut st = jsonnet_analyze::St::new(init);
    assert!(!self.inputs.is_empty(), "must have an Input to check");
    for input in self.inputs {
      input.check_with(&mut st, &mut fs, pwd.as_clean_path());
    }
  }
}

#[must_use]
#[derive(Default)]
pub(crate) struct Input<'a> {
  jsonnet: FxHashMap<&'a str, JsonnetInput<'a>>,
  raw: FxHashMap<&'a str, &'a str>,
  to_add: FxHashSet<&'a str>,
  to_remove: FxHashSet<&'a str>,
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

  pub(crate) fn remove(mut self, path: &'a str) -> Self {
    assert!(self.to_remove.insert(path));
    self
  }

  pub(crate) fn check(self) {
    MultiInput::default().with_input(self).check();
  }

  fn check_with(
    self,
    st: &mut jsonnet_analyze::St,
    fs: &mut paths::MemoryFileSystem,
    pwd: &paths::CleanPath,
  ) {
    let files = std::iter::empty()
      .chain(self.jsonnet.iter().map(|(&path, jsonnet)| (path, jsonnet.text)))
      .chain(self.raw.iter().map(|(&path, &text)| (path, text)))
      .map(|(path, text)| (pwd.join(path), text.to_owned()));
    // keep any existing files
    fs.inner.extend(files);

    assert!(!self.to_add.is_empty(), "must call .add() or .add_all() on the Input");
    let to_add: Vec<_> = self.to_add.iter().map(|&path| pwd.join(path)).collect();
    let to_remove: Vec<_> = self.to_remove.iter().map(|&path| pwd.join(path)).collect();

    let mut ds_map = st.update_many(fs, to_remove, to_add);
    let expects = self.jsonnet.iter().map(|(&path, jsonnet)| {
      let path = pwd.join(path);
      let path = st.path_id(path);
      let ex_file = expect::File::new(jsonnet.text);
      (path, ex_file)
    });
    let expects: paths::PathMap<_> = expects.collect();

    for (&path_str, jsonnet) in &self.jsonnet {
      let path = pwd.join(path_str);
      let path = st.path_id(path);
      let mut ds: FxHashMap<_, _> =
        ds_map.remove(&path).into_iter().flatten().map(|x| (x.range, x.message)).collect();

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
            assert_eq!(expect::Kind::Def, def_ex.kind, "{path_str}: not a def");
            assert_eq!(def_ex.msg, ex.msg, "{path_str}: mismatched uses");
          }
          expect::Kind::Diagnostic => {
            let range = text_pos::RangeUtf16 {
              start: text_pos::PositionUtf16 { line: region.line, col: region.col_start },
              end: text_pos::PositionUtf16 { line: region.line, col: region.col_end },
            };
            let got = ds.remove(&range).expect("no diagnostic at range");
            let want = if ex.msg == "<eval>" {
              assert_eq!(OutcomeKind::Error, jsonnet.kind, "{path_str}: mismatched outcome kind");
              jsonnet.outcome
            } else {
              ex.msg.as_str()
            };
            assert_eq!(want, got, "{path_str}: mismatched diagnostic");
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
            panic!("{path_str}: mismatched manifest\nwant: {want}\ngot:  {got}");
          }
        }

        (OutcomeKind::String, Ok(got)) => got.assert_is_str(st.strings(), jsonnet.outcome),

        (OutcomeKind::Error, Err(err)) => {
          let got = err.display(st.strings(), st.paths(), Some(pwd)).to_string();
          assert_eq!(jsonnet.outcome, got.as_str(), "{path_str}: mismatched errors");
        }

        (OutcomeKind::Error, Ok(got)) => panic!("{path_str}: no error, got json: {got:?}"),
        (OutcomeKind::Manifest | OutcomeKind::String, Err(err)) => {
          let got = err.display(st.strings(), st.paths(), Some(pwd)).to_string();
          panic!("{path_str}: error: {got:?}");
        }
      }

      if let Some((range, msg)) = ds.iter().next() {
        let n = ds.len();
        panic!("{path_str} still has {n} diagnostics, e.g. {range}: {msg}");
      }
    }

    if let Some((path, ds)) = ds_map.iter().next() {
      let d = ds.first().expect("empty ds");
      let n = ds_map.len();
      let path = st.paths().get_path(*path).as_path().display();
      panic!("still have {n} files with diagnostics, e.g. {path}: {d:?}");
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum OutcomeKind {
  Manifest,
  String,
  Error,
}
