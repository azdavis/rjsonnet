use jsonnet_desugar::Desugar;
use jsonnet_eval::manifest;
use jsonnet_expr::{Number, Prim};

pub(crate) fn num(n: f64) -> manifest::Val {
  manifest::Val::Prim(Prim::Number(Number::try_from(n).unwrap()))
}

pub(crate) fn exec(s: &str) -> (Desugar, jsonnet_eval::val::Val) {
  let lex = jsonnet_lex::get(s);
  if let Some(e) = lex.errors.first() {
    panic!("lex error: {e}");
  }
  let parse = jsonnet_parse::get(&lex.tokens);
  if let Some(e) = parse.errors.first() {
    panic!("parse error: {e:?}");
  }
  let desugar = jsonnet_desugar::get(parse.root);
  if let Some(e) = desugar.errors.first() {
    panic!("desugar error: {e:?}");
  }
  let mut st = jsonnet_statics::St::default();
  let cx = jsonnet_statics::Cx::default();
  jsonnet_statics::check(&mut st, &cx, &desugar.arenas, desugar.top);
  let statics_errors = st.finish();
  if let Some(e) = statics_errors.first() {
    let e = e.display(&desugar.arenas.str);
    panic!("statics error: {e}");
  }
  let env = jsonnet_eval::val::Env::default();
  let val = jsonnet_eval::exec::get(&env, &desugar.arenas, desugar.top);
  (desugar, val.expect("exec error"))
}

/// TODO have this take the wanted val and assert equal to gotten val?
pub(crate) fn manifest_raw(s: &str) -> (Desugar, manifest::Val) {
  let (desugar, val) = exec(s);
  let val = manifest::get(&desugar.arenas, val);
  (desugar, val.expect("manifest error"))
}

#[allow(clippy::needless_pass_by_value)]
pub(crate) fn manifest(s: &str, want: manifest::Val) {
  let (desugar, val) = exec(s);
  let val = manifest::get(&desugar.arenas, val);
  let got = val.expect("manifest error");
  assert_eq!(want, got);
}
