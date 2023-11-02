use crate::check::manifest;
use jsonnet_eval::manifest::Val;
use std::collections::HashMap;

#[test]
fn empty() {
  let want = Val::Object(HashMap::default());
  let got = manifest("{}");
  assert_eq!(want, got);
}

#[test]
fn non_empty() {
  let got = manifest(
    r#"
{
  num: 1,
  bool: true,
  str: "bar",
  "foo quz": null,
}
"#,
  );
  assert!(matches!(got, Val::Object(_)));
}

#[test]
#[should_panic = "+ for non-prim"]
fn implicit_plus() {
  manifest(
    r#"
{ a: 1 } { b: 2 }
"#,
  );
}
