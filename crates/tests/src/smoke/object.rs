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
fn self_() {
  manifest(
    r"
{
  a: 3,
  b: self.a + 1,
}
",
    r#"
{
  "a": 3,
  "b": 4
}
"#,
  );
}

#[test]
fn super_() {
  manifest(
    r"
local base = {
  a: 3,
  b: self.a + 1,
};

base + {
  a: 5,
  super_a: super.a,
  super_b: super.b,
}
",
    r#"
{
  "a": 5,
  "b": 6,
  "super_a": 3,
  "super_b": 6
}
"#,
  );
}

#[test]
fn explicit_plus() {
  manifest(
    r"
{ a: 1 } + { b: 2 }
",
    r#"
{ "a": 1, "b": 2 }
"#,
  );
}

#[test]
fn implicit_plus() {
  manifest(
    r"
{ a: 1 } { b: 2 }
",
    r#"
{ "a": 1, "b": 2 }
"#,
  );
}
