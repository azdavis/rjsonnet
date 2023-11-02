use crate::check::{manifest, manifest_raw};
use jsonnet_eval::manifest::Val;
use std::collections::HashMap;

#[test]
fn empty() {
  manifest("{}", Val::Object(HashMap::default()));
}

#[test]
fn non_empty() {
  let (_, got) = manifest_raw(
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
  manifest_raw(
    r#"
{ a: 1 } { b: 2 }
"#,
  );
}
