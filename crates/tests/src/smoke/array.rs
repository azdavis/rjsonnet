use crate::check::{manifest, manifest_raw, num};
use jsonnet_eval::manifest::Val;
use jsonnet_expr::Prim;

#[test]
fn empty() {
  manifest("[]", Val::Array(Vec::new()));
}

#[test]
fn non_empty() {
  let (mut desugar, got) = manifest_raw(
    r#"
[1, true, "foo"]
"#,
  );
  let str = desugar.arenas.str.insert("foo".to_owned().into_boxed_str());
  let want = Val::Array(vec![num(1.0), Val::Prim(Prim::Bool(true)), Val::Prim(Prim::String(str))]);
  assert_eq!(want, got);
}
