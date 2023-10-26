pub(crate) fn exec(s: &str) -> (jsonnet_desugar::Desugar, jsonnet_eval::val::Val) {
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
  if let Some(&(_, e)) = statics_errors.first() {
    panic!("statics error: {e}");
  }
  let env = jsonnet_eval::val::Env::default();
  let val = jsonnet_eval::exec::get(&env, &desugar.arenas, desugar.top);
  (desugar, val.expect("exec error"))
}

pub(crate) fn manifest(s: &str) -> jsonnet_eval::manifest::Val {
  let (desugar, val) = exec(s);
  let val = jsonnet_eval::manifest::get(&desugar.arenas, val);
  val.expect("manifest error")
}
