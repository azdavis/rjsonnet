//! Tests for objects, i.e. the things surrounded by `{}`.

use crate::check::JsonnetInput;

mod assert;
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
##^^^^^^^ err: no such field: `foo`

f(true)
"#,
    "3",
  )
  .check();
}

#[test]
fn array_self() {
  JsonnetInput::manifest(
    r#"
{
  foo: [
    { kind: 'Soda', qty: 2 },
  ],
  quz: self.foo,
}
"#,
    r#"
{
  "foo": [{ "kind": "Soda", "qty": 2 }],
  "quz": [{ "kind": "Soda", "qty": 2 }]
}
"#,
  )
  .check();
}

#[test]
fn eq_hidden() {
  JsonnetInput::manifest(
    r#"
{ a: 1, b:: 2 } == { a: 1 }
"#,
    "true",
  )
  .check();
}

#[test]
fn self_local() {
  JsonnetInput::manifest(
    r#"
{
  a:
    local x = self;
    x.b + 1,
  b: 3,
}
"#,
    r#"
{
  "a": 4,
  "b": 3
}
"#,
  )
  .check();
}

#[test]
fn forbid_extra() {
  JsonnetInput::manifest_or_fn(
    r#"
local f(obj) =
##    ^ type: (obj: { foo: number }) => number
  assert std.isObject(obj);
  assert std.isNumber(obj.foo);
  assert std.length(obj) == 1;
  obj.foo;

function()
  local arg = { foo: 1, bar: "hi" };

  f(arg)
##  ^^^ err: incompatible types; expected `{ foo: number }`; found `{ foo: number, bar: string }`
  "#,
  )
  .check();
}
#[test]
fn allow_unknown() {
  JsonnetInput::manifest_or_fn(
    r#"
local f(obj) =
##    ^ type: (obj: { foo: number }) => number
  assert std.isObject(obj);
  assert std.isNumber(obj.foo);
  assert std.length(obj) == 1;
  obj.foo;

function(arg)
  assert std.isObject(arg);

  f(arg)
"#,
  )
  .check();
}

/// TODO fix
#[test]
#[should_panic = "unreachable code"]
fn for_comp_obj_values() {
  JsonnetInput::manifest(
    r#"
local f(obj, field) =
  std.all([std.isNumber(val[field]) for val in std.objectValues(obj)]);

[f({}, 'e'), f({a: {e: 3}}, 'e'), f({a: {e: false}}, 'e')]
"#,
    r#"[true, true, false]"#,
  )
  .check();
}

#[test]
#[should_panic = "`super` must be used with `.`, `[]`, or `in`"]
fn in_super() {
  JsonnetInput::manifest(
    r#"
{
  a: "hi",
} + {
  b: "a" in super,
  c: "b" in super,
}
"#,
    r#"
{
  "a": "hi",
  "b": true,
  "c": false
}
"#,
  )
  .check();
}
