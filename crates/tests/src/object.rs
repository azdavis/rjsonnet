//! Tests for objects, i.e. the things surrounded by `{}`.

use crate::check::JsonnetInput;

mod comprehension;
mod plus;

#[test]
fn empty() {
  JsonnetInput::manifest_self("{}").check();
}

#[test]
fn non_empty() {
  JsonnetInput::manifest(
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
  )
  .check();
}

#[test]
fn self_() {
  JsonnetInput::manifest(
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
  )
  .check();
}

#[test]
fn super_() {
  JsonnetInput::manifest(
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
  )
  .check();
}

#[test]
fn self_2() {
  JsonnetInput::manifest(
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
  )
  .check();
}

#[test]
fn self_3() {
  JsonnetInput::manifest(
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
  )
  .check();
}

#[test]
fn root() {
  JsonnetInput::manifest(
    r#"
{
  foo: 1,
  bar: $.foo + 2
}
"#,
    r#"
{
  "foo": 1,
  "bar": 3
}
"#,
  )
  .check();
}

#[test]
fn union_field_get_all_have() {
  JsonnetInput::manifest(
    r#"
local f(x) =
  local obj =
    if x then
      { foo: 3 }
    else
      { foo: "hi" };
  obj.foo;
##    ^^^ hover: number | string

f(true)
"#,
    "3",
  )
  .check();
}

#[test]
fn union_field_get_some_missing() {
  JsonnetInput::manifest(
    r#"
local f(x) =
  local obj =
    if x then
      { foo: 3 }
    else
      {};
  obj.foo;
##^^^^^^^ diagnostic: no such field: `foo`

f(true)
"#,
    "3",
  )
  .check();
}
