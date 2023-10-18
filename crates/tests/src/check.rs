pub(crate) fn pass(s: &str) {
  let lex = jsonnet_lex::get(s);
  let parse = jsonnet_parse::get(&lex.tokens);
  let desugar = jsonnet_desugar::get(parse.root);
  let mut st = jsonnet_statics::St::default();
  let cx = jsonnet_statics::Cx::default();
  jsonnet_statics::check(&mut st, &cx, &desugar.arenas, desugar.top);
  let statics_errors = st.finish();
  let env = jsonnet_val::Env::default();
  let eval = jsonnet_dynamics::eval(&env, &desugar.arenas, desugar.top);
  assert!(lex.errors.is_empty());
  assert!(parse.errors.is_empty());
  assert!(desugar.errors.is_empty());
  assert!(statics_errors.is_empty());
  assert!(eval.is_ok());
}
