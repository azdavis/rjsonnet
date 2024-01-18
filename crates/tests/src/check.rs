use jsonnet_desugar::FileSystem;
use paths::FileSystem as _;
use rustc_hash::FxHashMap;
use std::path::Path;

#[derive(Debug, Default)]
struct MemoryFs(paths::MemoryFileSystem);

impl FileSystem for MemoryFs {
  fn canonicalize(&self, p: &Path) -> std::io::Result<paths::CanonicalPathBuf> {
    self.0.canonicalize(p)
  }
}

struct FileArtifacts {
  lex_errors: Vec<jsonnet_lex::Error>,
  parse: jsonnet_parse::Parse,
  statics_errors: Vec<jsonnet_statics::error::Error>,
  desugar: jsonnet_desugar::Desugar,
}

impl FileArtifacts {
  fn get(fs: &MemoryFs, path: &Path) -> Self {
    let s = fs.0.read_to_string(path).expect("io error");
    let lex = jsonnet_lex::get(s.as_str());
    let parse = jsonnet_parse::get(&lex.tokens);
    let p = path.parent().expect("no parent");
    let desugar = jsonnet_desugar::get(p, &[], fs, parse.root.clone());
    let statics_errors = jsonnet_statics::get(&desugar.arenas, desugar.top);
    Self { lex_errors: lex.errors, parse, statics_errors, desugar }
  }

  fn check(&self) {
    if let Some(e) = self.lex_errors.first() {
      panic!("lex error: {e}");
    }
    if let Some(e) = self.parse.errors.first() {
      panic!("parse error: {e}");
    }
    if let Some(e) = self.desugar.errors.first() {
      panic!("desugar error: {e}");
    }
    if let Some(e) = self.statics_errors.first() {
      let e = e.display(&self.desugar.arenas.str);
      panic!("statics error: {e}");
    }
  }
}

struct Files {
  fs: MemoryFs,
  artifacts: jsonnet_expr::Artifacts,
  map: paths::PathMap<jsonnet_eval::JsonnetFile>,
}

impl Files {
  fn get(ss: Vec<(&str, &str)>) -> Self {
    let m: FxHashMap<_, _> =
      ss.iter().map(|&(p, s)| (Path::new(p).to_owned(), s.to_owned())).collect();
    let mut ret = Self {
      fs: MemoryFs(paths::MemoryFileSystem::new(m)),
      artifacts: jsonnet_expr::Artifacts::default(),
      map: paths::PathMap::default(),
    };
    for (p, _) in ss {
      let p = Path::new(p);
      let a = FileArtifacts::get(&ret.fs, p);
      a.check();
      let mut f = jsonnet_eval::JsonnetFile { expr_ar: a.desugar.arenas.expr, top: a.desugar.top };
      let a = jsonnet_expr::Artifacts { paths: a.desugar.ps, strings: a.desugar.arenas.str };
      jsonnet_expr::combine::get(&mut ret.artifacts, a, &mut f.expr_ar);
      let p = ret.path_id(p);
      ret.map.insert(p, f);
    }
    ret
  }

  fn cx(&self) -> jsonnet_eval::Cx<'_> {
    jsonnet_eval::Cx { jsonnet_files: &self.map, str_ar: &self.artifacts.strings }
  }

  fn path_id(&mut self, path: &Path) -> paths::PathId {
    let p = self.fs.canonicalize(path).expect("canonicalize");
    self.artifacts.paths.get_id(&p)
  }
}

const DEFAULT_FILE_NAME: &str = "f.jsonnet";

fn exec(s: &str) -> (Files, jsonnet_eval::error::Result<jsonnet_eval::Jsonnet>) {
  let mut files = Files::get(vec![(DEFAULT_FILE_NAME, s)]);
  let p = files.path_id(Path::new(DEFAULT_FILE_NAME));
  let val = jsonnet_eval::get_exec(files.cx(), p);
  (files, val)
}

fn manifest_raw(s: &str) -> (Files, jsonnet_eval::Json) {
  let (files, val) = exec(s);
  let val = match val {
    Ok(x) => x,
    Err(e) => panic!("exec error: {}", e.display(&files.artifacts.strings)),
  };
  let val = match jsonnet_eval::get_manifest(files.cx(), val) {
    Ok(x) => x,
    Err(e) => panic!("manifest error: {}", e.display(&files.artifacts.strings)),
  };
  (files, val)
}

/// tests that `jsonnet` execution results in an error whose message is `want`.
pub(crate) fn exec_err(jsonnet: &str, want: &str) {
  let (files, a) = exec(jsonnet);
  let err = a.expect_err("no error");
  let got = err.display(&files.artifacts.strings).to_string();
  assert_eq!(want, got.as_str());
}

/// tests that `jsonnet` manifests to the `json`.
pub(crate) fn manifest(jsonnet: &str, json: &str) {
  let (files, got) = manifest_raw(jsonnet);
  let want: serde_json::Value = serde_json::from_str(json).unwrap();
  let want = jsonnet_eval::Json::from_serde(&files.artifacts.strings, want);
  if want != got {
    let want = want.display(&files.artifacts.strings);
    let got = got.display(&files.artifacts.strings);
    panic!("want: {want}\ngot:  {got}");
  }
}

/// tests that for each triple of (filename, jsonnet, json), each jsonnet manifests to its json.
pub(crate) fn manifest_many(input: &[(&str, &str, &str)]) {
  let mut files = Files::get(input.iter().map(|&(p, jsonnet, _)| (p, jsonnet)).collect());
  for &(p, _, json) in input {
    let p = files.path_id(Path::new(p));
    let got = jsonnet_eval::get_exec(files.cx(), p);
    let got = match got {
      Ok(x) => x,
      Err(e) => panic!("exec error: {}", e.display(&files.artifacts.strings)),
    };
    let got = match jsonnet_eval::get_manifest(files.cx(), got) {
      Ok(x) => x,
      Err(e) => panic!("manifest error: {}", e.display(&files.artifacts.strings)),
    };
    let want: serde_json::Value = serde_json::from_str(json).unwrap();
    let want = jsonnet_eval::Json::from_serde(&files.artifacts.strings, want);
    if want != got {
      let want = want.display(&files.artifacts.strings);
      let got = got.display(&files.artifacts.strings);
      panic!("want: {want}\ngot:  {got}");
    }
  }
}

/// tests that `s`, when treated as either jsonnet or json, manifests to the same thing.
pub(crate) fn manifest_self(s: &str) {
  manifest(s, s);
}

/// tests that `jsonnet` manifests to the string `want`.
///
/// NOTE: `want` is NOT interpreted as JSON.
pub(crate) fn manifest_str(jsonnet: &str, want: &str) {
  let (mut files, got) = manifest_raw(jsonnet);
  let want = files.artifacts.strings.str(want.to_owned().into_boxed_str());
  got.assert_is_str(&files.artifacts.strings, &want);
}
