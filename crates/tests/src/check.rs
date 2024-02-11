use paths::FileSystem;
use rustc_hash::FxHashMap;
use std::path::{Path, PathBuf};

const DEFAULT_FILE_NAME: &str = "f.jsonnet";

fn mk_st<'a, I>(iter: I) -> (paths::MemoryFileSystem, jsonnet_analyze::St)
where
  I: Iterator<Item = (&'a str, &'a str)> + Clone,
{
  let map: FxHashMap<_, _> =
    iter.clone().map(|(path, contents)| (PathBuf::from(path), contents.to_owned())).collect();
  let fs = paths::MemoryFileSystem::new(map);
  let mut ret = jsonnet_analyze::St::default();
  let add = iter.map(|(path, _)| PathBuf::from(path)).collect();
  for (path, ds) in ret.update_many(&fs, Vec::new(), add) {
    if let Some(d) = ds.first() {
      let p = ret.paths().get_path(path);
      panic!("{} at {}: diagnostic: {}", p.as_path().display(), d.range, d.message)
    }
  }
  (fs, ret)
}

fn get_json(st: &jsonnet_analyze::St, p: paths::PathId) -> &jsonnet_eval::Json {
  match st.get_json(p) {
    Ok(x) => x,
    Err(e) => panic!("exec/manifest error: {}", e.display(st.strings(), st.paths())),
  }
}

/// tests that for each triple of (filename, jsonnet, json), each jsonnet manifests to its json.
pub(crate) fn manifest_many(input: &[(&str, &str, &str)]) {
  let (fs, mut st) = mk_st(input.iter().map(|&(path, jsonnet, _)| (path, jsonnet)));
  for &(p, _, json) in input {
    let p = fs.canonicalize(Path::new(p)).expect("canonicalize");
    let p = st.path_id(p);
    let got = get_json(&st, p);
    let want: serde_json::Value = serde_json::from_str(json).expect("test input json");
    let want = jsonnet_eval::Json::from_serde(st.strings(), want);
    if want != *got {
      let want = want.display(st.strings());
      let got = got.display(st.strings());
      panic!("want: {want}\ngot:  {got}");
    }
  }
}

/// tests that `jsonnet` manifests to the `json`.
pub(crate) fn manifest(jsonnet: &str, json: &str) {
  manifest_many(&[(DEFAULT_FILE_NAME, jsonnet, json)]);
}

/// tests that `s`, when treated as either jsonnet or json, manifests to the same thing.
pub(crate) fn manifest_self(s: &str) {
  manifest(s, s);
}

/// tests that `jsonnet` manifests to the string `want`.
///
/// NOTE: `want` is NOT interpreted as JSON.
pub(crate) fn manifest_str(jsonnet: &str, want: &str) {
  let (fs, mut st) = mk_st(std::iter::once((DEFAULT_FILE_NAME, jsonnet)));
  let want = st.strings_mut().str(want.to_owned().into_boxed_str());
  let p = fs.canonicalize(Path::new(DEFAULT_FILE_NAME)).expect("canonicalize");
  let p = st.path_id(p);
  let got = get_json(&st, p);
  got.assert_is_str(st.strings(), &want);
}

/// tests that `jsonnet` execution results in an error whose message is `want`.
pub(crate) fn exec_err(jsonnet: &str, want: &str) {
  let (fs, mut st) = mk_st(std::iter::once((DEFAULT_FILE_NAME, jsonnet)));
  let p = fs.canonicalize(Path::new(DEFAULT_FILE_NAME)).expect("canonicalize");
  let p = st.path_id(p);
  let err = st.get_json(p).as_ref().expect_err("no error");
  let got = err.display(st.strings(), st.paths()).to_string();
  assert_eq!(want, got.as_str());
}
