use crate::check::{go, manifest, manifest_raw};
use jsonnet_eval::manifest::Val;
use rustc_hash::FxHashMap;

#[test]
fn empty() {
  manifest("{}", Val::Object(FxHashMap::default()));
}

#[test]
fn non_empty() {
  let jsonnet = r#"
{
  num: 1,
  bool: true,
  str: "bar",
  "foo quz": null,
}
"#;
  let json = r#"
{
  "num": 1,
  "bool": true,
  "str": "bar",
  "foo quz": null
}
"#;
  go(jsonnet, json);
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
