//! Unused bindings.

mod remove;

use crate::check::JsonnetInput;

#[test]
fn fn_param() {
  JsonnetInput::manifest(
    r"
local uh = function(x) 3;
##                  ^ err: unused variable: `x`
uh(4)
",
    "3",
  )
  .check();
}

#[test]
fn local() {
  JsonnetInput::manifest(
    r"
local y = 3;
##    ^ err: unused variable: `y`
{}
",
    "{}",
  )
  .check();
}

#[test]
fn local_param() {
  JsonnetInput::manifest(
    r"
local f(x) = 3;
##      ^ err: unused variable: `x`
f(0)
",
    "3",
  )
  .check();
}

#[test]
fn in_all_fields() {
  JsonnetInput::manifest(
    r#"
{
  local a = 1,
##      ^ err: unused variable: `a`
  b: "hi",
  c: 3,
}
"#,
    r#"
{
  "b": "hi",
  "c": 3
}
"#,
  )
  .check();
}

// should NOT trigger unused
#[test]
fn in_some_fields() {
  JsonnetInput::manifest(
    r#"
{
  local a = 1,
  b: "hi",
  c: a + 2,
}
"#,
    r#"
{
  "b": "hi",
  "c": 3
}
"#,
  )
  .check();
}

#[test]
fn no_field() {
  JsonnetInput::manifest(
    r#"
{
  local a = 1,
##      ^ err: unused variable: `a`
}
"#,
    "{}",
  )
  .check();
}

#[test]
fn array_comp() {
  JsonnetInput::manifest(
    r#"
[3 for x in []]
##     ^ err: unused variable: `x`
"#,
    "[]",
  )
  .check();
}

#[test]
fn object_comp() {
  JsonnetInput::eval_error(
    r#"
{
  ["foo"]: 0 for x in ["a", "b"]
##^^^^^^^^^^ err: unused variable: `x`
}
"#,
    "duplicate field: `foo`",
  )
  .check();
}

#[test]
fn object_comp_only_key() {
  JsonnetInput::manifest(
    r#"
{
  [x]: 0 for x in ["a", "b"]
}
"#,
    r#"
{
  "a": 0,
  "b": 0
}
"#,
  )
  .check();
}

#[test]
fn object_comp_only_val() {
  JsonnetInput::manifest(
    r#"
{
  ["foo"]: x for x in [0]
}
"#,
    r#"
{
  "foo": 0
}
"#,
  )
  .check();
}

#[test]
fn object_comp_extra_local() {
  JsonnetInput::manifest(
    r#"
{
  local no = k,
  [k]: false,
##^^^^^^^^^^ err: unused variable: `no`
  for k in ["a", "b"]
}
"#,
    r#"
{
  "a": false,
  "b": false
}
"#,
  )
  .check();
}

#[test]
fn underscore_ok() {
  JsonnetInput::manifest(
    r#"
local _no = 1;
2
"#,
    "2",
  )
  .check();
}
