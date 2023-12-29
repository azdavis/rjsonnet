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
  self_a: self.a,
  self_b: self.b,
  super_a: super.a,
  super_b: super.b,
}
",
    r#"
{
  "a": 5,
  "b": 6,
  "self_a": 5,
  "self_b": 6,
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
{ a: 1, b: 2 } + { a: 3, c: 4 }
",
    r#"
{ "a": 3, "b": 2, "c": 4 }
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

#[test]
fn self_2() {
  manifest(
    r"
local x = {
  a: 3,
  b: self.a,
};
{
  a: 4,
  x: x,
}
",
    r#"
{
  "a": 4,
  "x": {
    "a": 3,
    "b": 3
  }
}
"#,
  );
}

#[test]
fn self_3() {
  manifest(
    r"
{
  a: 1,
  inner: local this = self; {
    a: 2,
    this_a: this.a,
    self_a: self.a,
  },
  outer_self_a: self.a,
}
",
    r#"
{
  "a": 1,
  "inner": {
    "a": 2,
    "this_a": 1,
    "self_a": 2
  },
  "outer_self_a": 1
}
"#,
  );
}
