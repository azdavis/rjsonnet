//! Tests for object asserts, which appear in field position.

use crate::check::JsonnetInput;

#[test]
fn smoke_ok() {
  JsonnetInput::manifest(
    r#"
{ assert true : "bad", a: 5 }
"#,
    r#"
{
  "a": 5
}
"#,
  )
  .check();
}

#[test]
fn smoke_err() {
  JsonnetInput::eval_error(
    r#"
{ assert false : "bad", a: 5 }
"#,
    "bad",
  )
  .check();
}

/// TODO fix
#[test]
#[ignore = "stack overflow"]
fn self_ok() {
  JsonnetInput::manifest(
    r#"
{ assert self.a < 9 : "bad", a: 3 }
"#,
    r#"
{
  "a": 3
}
"#,
  )
  .check();
}

/// TODO fix
#[test]
#[ignore = "stack overflow"]
fn self_err() {
  JsonnetInput::manifest(
    r#"
{ assert self.a > 9 : "bad", a: 3 }
"#,
    "bad",
  )
  .check();
}

#[test]
fn lazy_manifest() {
  JsonnetInput::manifest(
    r#"
{
  assert std.length([error "oops"]) == 1 : "bad",
  a: 2 + 3,
}
"#,
    r#"
{
  "a": 5
}
"#,
  )
  .check();
}

#[test]
fn lazy_field_get() {
  JsonnetInput::manifest(
    r#"
local obj = {
  assert std.length([error "oops"]) == 1 : "bad",
  a: 2 + 3,
};
obj.a
"#,
    "5",
  )
  .check();
}

#[test]
#[should_panic = "not yet implemented: object-object equality"]
fn eq_checks() {
  JsonnetInput::eval_error(
    r#"
{ assert false : "bad", a: 1 } == { a: 1 }
"#,
    "bad",
  )
  .check();
}

#[test]
fn std_object_has_yes_contains_no_check() {
  JsonnetInput::manifest(
    r#"
local obj = { assert false, a: 1 };
std.objectHas(obj, "a")
"#,
    "true",
  )
  .check();
}

#[test]
fn std_object_has_no_contains_no_check() {
  JsonnetInput::manifest(
    r#"
local obj = { assert false, a: 1 };
std.objectHas(obj, "b")
"#,
    "false",
  )
  .check();
}

#[test]
#[should_panic = "not yet implemented: get"]
fn std_get_no_default_yes_contains_yes_check() {
  JsonnetInput::eval_error(
    r#"
local obj = { assert false : "bad", a: 1 };
std.get(obj, "a")
"#,
    "bad",
  )
  .check();
}

#[test]
#[should_panic = "not yet implemented: get"]
fn std_get_yes_default_yes_contains_yes_check() {
  JsonnetInput::eval_error(
    r#"
local obj = { assert false : "bad", a: 1 };
std.get(obj, "a", 2)
"#,
    "bad",
  )
  .check();
}

#[test]
#[should_panic = "not yet implemented: get"]
fn std_get_yes_default_no_contains_no_check() {
  JsonnetInput::manifest(
    r#"
local obj = { assert false : "bad", a: 1 };
std.get(obj, "b", 2)
"#,
    "2",
  )
  .check();
}

#[test]
fn field_get_yes() {
  JsonnetInput::eval_error(
    r#"
local obj = { assert false : "bad", a: 1 };
obj.a
"#,
    "bad",
  )
  .check();
}
