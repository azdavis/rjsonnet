use rustc_hash::FxHashMap;
use std::path::{Path, PathBuf};

const DEFAULT_FILE_NAME: &str = "f.jsonnet";

pub(crate) type St = jsonnet_analyze::St<paths::MemoryFileSystem>;

fn mk_st<'a, I>(iter: I) -> St
where
  I: Iterator<Item = (&'a str, &'a str)> + Clone,
{
  let map: FxHashMap<_, _> =
    iter.clone().map(|(path, contents)| (PathBuf::from(path), contents.to_owned())).collect();
  let fs = paths::MemoryFileSystem::new(map);
  let mut ret = St::new(fs);
  let add = iter.map(|(path, _)| PathBuf::from(path)).collect();
  ret.update_many(Vec::new(), add);
  ret
}

fn manifest_raw(st: &mut St, name: &str) -> jsonnet_eval::Json {
  let p = st.path_id(Path::new(name));
  let val = jsonnet_eval::get_exec(st.cx(), p);
  let val = match val {
    Ok(x) => x,
    Err(e) => panic!("exec error: {}", e.display(st.strings())),
  };
  match jsonnet_eval::get_manifest(st.cx(), val) {
    Ok(x) => x,
    Err(e) => panic!("manifest error: {}", e.display(st.strings())),
  }
}

/// tests that for each triple of (filename, jsonnet, json), each jsonnet manifests to its json.
pub(crate) fn manifest_many(input: &[(&str, &str, &str)]) {
  let mut st = mk_st(input.iter().map(|&(path, jsonnet, _)| (path, jsonnet)));
  for &(p, _, json) in input {
    let got = manifest_raw(&mut st, p);
    let want: serde_json::Value = serde_json::from_str(json).unwrap();
    let want = jsonnet_eval::Json::from_serde(st.strings(), want);
    if want != got {
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
  let mut st = mk_st(std::iter::once((DEFAULT_FILE_NAME, jsonnet)));
  let got = manifest_raw(&mut st, DEFAULT_FILE_NAME);
  let want = st.strings_mut().str(want.to_owned().into_boxed_str());
  got.assert_is_str(st.strings(), &want);
}

/// tests that `jsonnet` execution results in an error whose message is `want`.
pub(crate) fn exec_err(jsonnet: &str, want: &str) {
  let mut st = mk_st(std::iter::once((DEFAULT_FILE_NAME, jsonnet)));
  let p = st.path_id(Path::new(DEFAULT_FILE_NAME));
  let a = jsonnet_eval::get_exec(st.cx(), p);
  let err = a.expect_err("no error");
  let got = err.display(st.strings()).to_string();
  assert_eq!(want, got.as_str());
}
