pub(crate) fn exec(s: &str) -> (jsonnet_desugar::Desugar, jsonnet_eval::val::Val) {
  let lex = jsonnet_lex::get(s);
  assert!(lex.errors.is_empty());
  let parse = jsonnet_parse::get(&lex.tokens);
  assert!(parse.errors.is_empty());
  let desugar = jsonnet_desugar::get(parse.root);
  assert!(desugar.errors.is_empty());
  let mut st = jsonnet_statics::St::default();
  let cx = jsonnet_statics::Cx::default();
  jsonnet_statics::check(&mut st, &cx, &desugar.arenas, desugar.top);
  let statics_errors = st.finish();
  assert!(statics_errors.is_empty());
  let env = jsonnet_eval::val::Env::default();
  let val = jsonnet_eval::exec::get(&env, &desugar.arenas, desugar.top);
  (desugar, val.unwrap())
}

pub(crate) fn manifest(s: &str) -> jsonnet_eval::manifest::Val {
  let (desugar, val) = exec(s);
  let val = jsonnet_eval::manifest::get(&desugar.arenas, val);
  val.unwrap()
}
