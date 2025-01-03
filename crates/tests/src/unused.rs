//! Unused bindings.

use crate::check::JsonnetInput;

#[test]
fn fn_param() {
  JsonnetInput::manifest(
    r"
local uh = function(x) 3;
##                  ^ diagnostic: unused variable: `x`
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
##    ^ diagnostic: unused variable: `y`
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
##      ^ diagnostic: unused variable: `x`
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
##      ^ diagnostic: unused variable: `a`
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

#[test]
fn no_field() {
  JsonnetInput::manifest(
    r#"
{
  local a = 1,
##      ^ diagnostic: unused variable: `a`
}
"#,
    "{}",
  )
  .check();
}

#[test]
fn array_comp() {
  JsonnetInput::eval_error(
    r#"
[3 for x in []]
##     ^ diagnostic: unused variable: `x`
"#,
    "not yet implemented: makeArray",
  )
  .check();
}

#[test]
fn object_comp() {
  JsonnetInput::eval_error(
    r#"
{
  local no = k,
  [k]: false,
##^^^^^^^^^^ diagnostic: unused variable: `no`
  for k in ["a", "b"]
}
"#,
    "not yet implemented: makeArray",
  )
  .check();
}

// TODO the var is used, but not in both places.
#[test]
#[should_panic = "unused variable: `x`"]
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

// TODO the var is used, but not in both places.
#[test]
#[should_panic = "unused variable: `x`"]
fn object_comp_only_val() {
  JsonnetInput::manifest(
    r#"
{
  ["foo"]: x for x in [0]
}
"#,
    r#"
{
  "foo": 0,
}
"#,
  )
  .check();
}
