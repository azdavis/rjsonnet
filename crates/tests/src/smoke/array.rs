use crate::check::manifest;
use jsonnet_eval::manifest::Val;
use jsonnet_expr::{Number, Prim};

#[test]
fn empty() {
  let want = Val::Array(Vec::new());
  let got = manifest("[]");
  assert_eq!(want, got);
}

#[test]
fn non_empty() {
  let got = manifest(
    r#"
[1, true, "foo"]
"#,
  );
  let Val::Array(vs) = &got else { panic!("not an array") };
  let [fst, snd, thd] = &vs[..] else { panic!("not length 3") };
  assert_eq!(Val::Prim(Prim::Number(Number::positive_one())), *fst);
  assert_eq!(Val::Prim(Prim::Bool(true)), *snd);
  assert!(matches!(*thd, Val::Prim(Prim::String(_))));
}
