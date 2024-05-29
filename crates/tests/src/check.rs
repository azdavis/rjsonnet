//! Testing infra.

// TODO remove when tests are back on
#![allow(unused)]

mod expect;

use lang_srv_state::State as _;
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

  #[track_caller]
  pub(crate) fn check(self) {
    _ = env_logger::builder().is_test(true).filter_level(log::LevelFilter::Debug).try_init();
    let mut fs = paths::MemoryFileSystem::default();
    let pwd = fs.current_dir().expect("no current dir for in-mem fs");
    let init = jsonnet_analyze::Init { relative_to: Some(pwd.clone()), ..Default::default() };
    let mut st = jsonnet_analyze::St::init(init);
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

  #[track_caller]
  pub(crate) fn check(self) {
    MultiInput::default().with_input(self).check();
  }

  #[allow(clippy::too_many_lines)]
  #[track_caller]
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

    for path in &to_remove {
      assert!(
        fs.inner.remove(path).is_some(),
        "{}: removed but non-existent",
        path.as_path().display()
      );
    }

    st.mark_as_updated(to_remove.into_iter().chain(to_add).collect());
    let expects = self.jsonnet.iter().map(|(&path, jsonnet)| {
      let path = pwd.join(path);
      let path = st.path_id(path);
      let ex_file = expect::File::new(jsonnet.text);
      (path, ex_file)
    });
    let expects: paths::PathMap<_> = expects.collect();

    for (&path_str, jsonnet) in &self.jsonnet {
      let path = pwd.join(path_str);
      let (path_id, ds) = st.open(fs, path.clone(), jsonnet.text.to_owned());
      let mut ds: FxHashMap<_, _> = ds.into_iter().map(|x| (x.range, x.message)).collect();

      st.get_all_deps(fs, path_id);

      let ex_file = &expects[&path_id];
      for (region, ex) in ex_file.iter() {
        match ex.kind {
          expect::Kind::Def => {}
          expect::Kind::Use => {
            let pos = text_pos::PositionUtf16 { line: region.line, col: region.col_start };
            let (def_path, range) = st.get_def(fs, path.clone(), pos).expect("no def");
            assert_eq!(range.start.line, range.end.line, "{path_str}: range spans many lines");
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
            let got = make_one_line(&got);
            let want = if ex.msg == "<eval>" {
              assert_eq!(
                OutcomeKind::EvalError,
                jsonnet.kind,
                "{path_str}: mismatched outcome kind"
              );
              jsonnet.outcome
            } else {
              ex.msg.as_str()
            };
            assert_eq!(want, got, "{path_str}: mismatched diagnostic");
          }
        }
      }

      if let Some((range, msg)) = ds.iter().next() {
        let n = ds.len();
        panic!("{path_str} still has {n} diagnostics, e.g. {range}: {msg}");
      }

      match (jsonnet.kind, st.get_json(path_id)) {
        (OutcomeKind::Manifest, Ok(got)) => {
          let want: serde_json::Value =
            serde_json::from_str(jsonnet.outcome).expect("test input json");
          let want = jsonnet_eval::Json::from_serde(st.strings(), want);
          if want != got {
            let want = want.display(st.strings());
            let got = got.display(st.strings());
            panic!("{path_str}: mismatched manifest\nwant: {want}\ngot:  {got}");
          }
        }

        (OutcomeKind::String, Ok(got)) => got.assert_is_str(st.strings(), jsonnet.outcome),

        (OutcomeKind::EvalError, Err(err)) => {
          let got = err.display(st.strings(), st.paths(), Some(pwd)).to_string();
          let got = make_one_line(&got);
          assert_eq!(jsonnet.outcome, got.as_str(), "{path_str}: mismatched errors");
        }

        (OutcomeKind::PreEvalError, Err(err)) => {
          assert!(
            jsonnet.outcome.is_empty(),
            "{}: unexpected outcome for pre eval error: {}",
            path_str,
            jsonnet.outcome
          );
          let jsonnet_eval::error::Error::NoPath(p) = err else {
            panic!("{path_str}: unexpected error kind: {err:?}");
          };
          let p_display = st.paths().get_path(p).as_path().display();
          assert_eq!(path_id, p, "{path_str}: mismatched no-path errors: got {p_display}");
        }

        (OutcomeKind::EvalError | OutcomeKind::PreEvalError, Ok(got)) => {
          panic!("{path_str}: no error, got json: {got:?}")
        }
        (OutcomeKind::Manifest | OutcomeKind::String, Err(err)) => {
          let got = err.display(st.strings(), st.paths(), Some(pwd)).to_string();
          panic!("{path_str}: error: {got:?}");
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

  pub(crate) fn eval_error(text: &'a str, message: &'a str) -> Self {
    Self { text, outcome: message, kind: OutcomeKind::EvalError }
  }

  pub(crate) fn pre_eval_error(text: &'a str) -> Self {
    Self { text, outcome: "", kind: OutcomeKind::PreEvalError }
  }

  #[track_caller]
  pub(crate) fn check(self) {
    Input::default().with_jsonnet(DEFAULT_PATH, self).add_all().check();
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum OutcomeKind {
  Manifest,
  String,
  EvalError,
  PreEvalError,
}

/// this is a bit annoying, but i don't want to pull in iter tools just for intersperse or join or
/// whatever it's called. the real long term solution to get rid of this is probably a setting for
/// the error displaying family of functions for how many lines of output we want.
fn make_one_line(s: &str) -> String {
  let mut ret = String::with_capacity(s.len());
  let mut first = true;
  for line in s.lines() {
    if first {
      first = false;
    } else {
      ret.push_str("; ");
    }
    ret.push_str(line.trim());
  }
  ret
}
