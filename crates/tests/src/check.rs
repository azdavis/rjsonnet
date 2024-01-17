use jsonnet_desugar::FileSystem;
use paths::FileSystem as _;
use rustc_hash::FxHashMap;

#[derive(Debug, Default)]
struct MemoryFs(paths::MemoryFileSystem);

impl FileSystem for MemoryFs {
  fn canonicalize(&self, p: &std::path::Path) -> std::io::Result<paths::CanonicalPathBuf> {
    self.0.canonicalize(p)
  }
}

struct Artifacts {
  lex_errors: Vec<jsonnet_lex::Error>,
  parse: jsonnet_parse::Parse,
  statics_errors: Vec<jsonnet_statics::error::Error>,
  desugar: jsonnet_desugar::Desugar,
  path: paths::PathId,
}

impl Artifacts {
  fn get(s: &str) -> Self {
    let lex = jsonnet_lex::get(s);
    let parse = jsonnet_parse::get(&lex.tokens);
    let p = std::path::Path::new("/f.jsonnet");
    let fs =
      MemoryFs(paths::MemoryFileSystem::new(FxHashMap::from_iter([(p.to_owned(), s.to_owned())])));
    let mut desugar = jsonnet_desugar::get(std::path::Path::new("/"), &[], &fs, parse.root.clone());
    let p = fs.canonicalize(p).expect("canonicalize");
    let p = desugar.ps.get_id(&p);
    let mut st = jsonnet_statics::St::default();
    let cx = jsonnet_statics::Cx::default();
    jsonnet_statics::check(&mut st, &cx, &desugar.arenas, desugar.top);
    let statics_errors = st.finish();
    Self { lex_errors: lex.errors, parse, statics_errors, desugar, path: p }
  }

  fn jsonnet_files(&self) -> paths::PathMap<jsonnet_eval::JsonnetFile<'_>> {
    paths::PathMap::from_iter([(
      self.path,
      jsonnet_eval::JsonnetFile { expr_ar: &self.desugar.arenas.expr, top: self.desugar.top },
    )])
  }

  fn cx<'a>(
    &'a self,
    jsonnet_files: &'a paths::PathMap<jsonnet_eval::JsonnetFile<'a>>,
  ) -> jsonnet_eval::Cx<'a> {
    jsonnet_eval::Cx { jsonnet_files, str_ar: &self.desugar.arenas.str }
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

fn exec(s: &str) -> (Artifacts, jsonnet_eval::error::Result<jsonnet_eval::Jsonnet>) {
  let art = Artifacts::get(s);
  art.check();
  let jf = art.jsonnet_files();
  let val = jsonnet_eval::get_exec(art.cx(&jf), art.path);
  (art, val)
}

fn manifest_raw(s: &str) -> (Artifacts, jsonnet_eval::Json) {
  let (art, val) = exec(s);
  let val = match val {
    Ok(x) => x,
    Err(e) => panic!("exec error: {}", e.display(&art.desugar.arenas.str)),
  };
  let jf = art.jsonnet_files();
  let val = match jsonnet_eval::get_manifest(art.cx(&jf), val) {
    Ok(x) => x,
    Err(e) => panic!("manifest error: {}", e.display(&art.desugar.arenas.str)),
  };
  (art, val)
}

/// tests that `jsonnet` execution results in an error whose message is `want`.
pub(crate) fn exec_err(jsonnet: &str, want: &str) {
  let (art, a) = exec(jsonnet);
  let err = a.expect_err("no error");
  let got = err.display(&art.desugar.arenas.str).to_string();
  assert_eq!(want, got.as_str());
}

/// tests that `jsonnet` manifests to the `json`.
pub(crate) fn manifest(jsonnet: &str, json: &str) {
  let want: serde_json::Value = serde_json::from_str(json).unwrap();
  let (art, got) = manifest_raw(jsonnet);
  let want = jsonnet_eval::Json::from_serde(&art.desugar.arenas.str, want);
  if want != got {
    let want = want.display(&art.desugar.arenas.str);
    let got = got.display(&art.desugar.arenas.str);
    panic!("want: {want}\ngot:  {got}");
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
  let (mut art, got) = manifest_raw(jsonnet);
  let want = art.desugar.arenas.str.str(want.to_owned().into_boxed_str());
  got.assert_is_str(&art.desugar.arenas.str, &want);
}
