use jsonnet_desugar::Desugar;
use jsonnet_eval::val::{json, jsonnet};
use jsonnet_expr::{Number, Prim};

fn exec(s: &str) -> (Desugar, jsonnet_eval::error::Result<jsonnet::Val>) {
  let lex = jsonnet_lex::get(s);
  if let Some(e) = lex.errors.first() {
    panic!("lex error: {e}");
  }
  let parse = jsonnet_parse::get(&lex.tokens);
  if let Some(e) = parse.errors.first() {
    panic!("parse error: {e}");
  }
  let desugar = jsonnet_desugar::get(parse.root);
  if let Some(e) = desugar.errors.first() {
    panic!("desugar error: {e}");
  }
  let mut st = jsonnet_statics::St::default();
  let cx = jsonnet_statics::Cx::default();
  jsonnet_statics::check(&mut st, &cx, &desugar.arenas, desugar.top);
  let statics_errors = st.finish();
  if let Some(e) = statics_errors.first() {
    let e = e.display(&desugar.arenas.str);
    panic!("statics error: {e}");
  }
  let env = jsonnet::Env::default();
  let empty = jsonnet::Object::default();
  let cx = jsonnet_eval::exec::Cx::new(&env, &empty);
  let val = jsonnet_eval::exec::get(cx, &desugar.arenas, desugar.top);
  (desugar, val)
}

fn manifest_raw(s: &str) -> (Desugar, json::Val) {
  let (desugar, val) = exec(s);
  let empty = jsonnet::Object::default();
  let val = match val {
    Ok(x) => x,
    Err(e) => panic!("exec error: {}", e.display(&desugar.arenas.str)),
  };
  let val = match jsonnet_eval::manifest::get(&desugar.arenas, &empty, val) {
    Ok(x) => x,
    Err(e) => panic!("manifest error: {}", e.display(&desugar.arenas.str)),
  };
  (desugar, val)
}

fn from_serde(ar: &jsonnet_expr::StrArena, serde: serde_json::Value) -> json::Val {
  match serde {
    serde_json::Value::Null => json::Val::Prim(Prim::Null),
    serde_json::Value::Bool(b) => json::Val::Prim(Prim::Bool(b)),
    serde_json::Value::Number(num) => {
      let num = num.as_f64().unwrap();
      let num = Number::try_from(num).unwrap();
      json::Val::Prim(Prim::Number(num))
    }
    serde_json::Value::String(str) => {
      let str = ar.str_shared(str.into_boxed_str());
      json::Val::Prim(Prim::String(str))
    }
    serde_json::Value::Array(vs) => {
      let iter = vs.into_iter().map(|v| from_serde(ar, v));
      json::Val::Array(iter.collect())
    }
    serde_json::Value::Object(map) => {
      let iter = map.into_iter().map(|(k, v)| {
        let k = ar.str_shared(k.into_boxed_str());
        let v = from_serde(ar, v);
        (k, v)
      });
      json::Val::Object(iter.collect())
    }
  }
}

/// tests that `jsonnet` execution results in an error whose message is `want`.
pub(crate) fn exec_err(jsonnet: &str, want: &str) {
  let (desugar, a) = exec(jsonnet);
  let err = a.expect_err("no error");
  let got = err.display(&desugar.arenas.str).to_string();
  assert_eq!(want, got.as_str());
}

/// tests that `jsonnet` manifests to the `json`.
pub(crate) fn manifest(jsonnet: &str, json: &str) {
  let want: serde_json::Value = serde_json::from_str(json).unwrap();
  let (desugar, got) = manifest_raw(jsonnet);
  let want = from_serde(&desugar.arenas.str, want);
  if want != got {
    let want = want.display(&desugar.arenas.str);
    let got = got.display(&desugar.arenas.str);
    panic!("want: {want}\ngot:  {got}");
  }
}

/// tests that `s`, when treated as either jsonnet or json, manifests to the same thing.
pub(crate) fn manifest_self(s: &str) {
  manifest(s, s);
}

/// tests that `jsonnet` manifests to the string `want`. NOTE: `want` is NOT interpreted as JSON.
pub(crate) fn manifest_str(jsonnet: &str, want: &str) {
  let (mut desugar, got) = manifest_raw(jsonnet);
  let want = desugar.arenas.str.str(want.to_owned().into_boxed_str());
  let json::Val::Prim(Prim::String(got)) = got else { panic!("did not get a String") };
  if want != got {
    let want = desugar.arenas.str.get(&want);
    let got = desugar.arenas.str.get(&got);
    panic!("want: {want:?}\ngot:  {got:?}")
  }
}
