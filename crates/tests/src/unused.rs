//! Unused bindings.

use crate::check::JsonnetInput;

#[test]
fn fn_param() {
  JsonnetInput::pre_eval_error(
    r"
local uh = function(x) 3;
##                  ^ diagnostic: unused: `x`
uh(4)
",
  )
  .check();
}

#[test]
fn local() {
  JsonnetInput::pre_eval_error(
    r"
local y = 3;
##    ^ diagnostic: unused: `y`
{}
",
  )
  .check();
}

#[test]
fn local_param() {
  JsonnetInput::pre_eval_error(
    r"
local f(x) = 3;
##      ^ diagnostic: unused: `x`
f(0)
",
  )
  .check();
}

#[test]
fn in_all_fields() {
  JsonnetInput::pre_eval_error(
    r#"
{
  local a = 1,
##      ^ diagnostic: unused: `a`
  b: "hi",
  c: 3,
}
"#,
  )
  .check();
}

#[test]
fn no_field() {
  JsonnetInput::pre_eval_error(
    r#"
{
  local a = 1,
##      ^ diagnostic: unused: `a`
}
"#,
  )
  .check();
}

#[test]
fn array_comp() {
  JsonnetInput::pre_eval_error(
    r#"
[3 for x in []]
##     ^ diagnostic: unused: `x`
"#,
  )
  .check();
}

#[test]
fn object_comp() {
  JsonnetInput::pre_eval_error(
    r#"
{
  local no = k,
  [k]: false,
##^^^^^^^^^^ diagnostic: unused: `no`
  for k in ["a", "b"]
}
"#,
  )
  .check();
}

// TODO the var is used, but not in both places.
#[test]
#[should_panic = "unused: `x`"]
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
#[should_panic = "unused: `x`"]
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
