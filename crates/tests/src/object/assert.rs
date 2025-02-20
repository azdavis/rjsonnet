//! Tests for object asserts, which appear in field position.

mod field_get;
mod std_get;

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

#[test]
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

#[test]
fn self_err() {
  JsonnetInput::eval_error(
    r#"
{ assert self.a > 9 : "bad", a: 3 }
"#,
    "bad",
  )
  .check();
}

#[test]
fn super_ok() {
  JsonnetInput::manifest(
    r#"
{ a: 10 } + { assert super.a > 5, assert self.a < 5, a: 2 }
"#,
    r#"
{
  "a": 2
}
"#,
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
fn does_not_force_all_fields() {
  JsonnetInput::manifest(
    r#"
local obj = { assert 1 + 1 == 2 : "object assert", a: error "a", b: 2 };
obj.b
"#,
    "2",
  )
  .check();
}
