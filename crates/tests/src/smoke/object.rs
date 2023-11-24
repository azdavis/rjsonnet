use crate::check::{manifest, manifest_self};

#[test]
fn empty() {
  manifest_self("{}");
}

#[test]
fn non_empty() {
  manifest(
    r#"
{
  num: 1,
  bool: true,
  str: "bar",
  "foo quz": null,
}
"#,
    r#"
{
  "num": 1,
  "bool": true,
  "str": "bar",
  "foo quz": null
}
"#,
  );
}

#[test]
#[should_panic = "+ for objects"]
fn explicit_plus() {
  manifest(
    r#"
{ a: 1 } + { b: 2 }
"#,
    r#"
{ "a": 1, "b": 2 }
"#,
  );
}

#[test]
#[should_panic = "+ for objects"]
fn implicit_plus() {
  manifest(
    r#"
{ a: 1 } { b: 2 }
"#,
    r#"
{ "a": 1, "b": 2 }
"#,
  );
}
