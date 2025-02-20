//! Tests for object asserts with `std.get`.

use crate::check::JsonnetInput;

#[test]
fn no_default_yes_contains_yes_check() {
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
fn yes_default_yes_contains_yes_check() {
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
fn yes_default_no_contains_no_check() {
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
fn yes_default_yes_contains_yes_hidden_no_inc_hidden_no_check() {
  JsonnetInput::manifest(
    r#"
local obj = { assert false : "bad", a:: 1 };
std.get(obj, "a", 2, inc_hidden=false)
"#,
    "2",
  )
  .check();
}

#[test]
fn yes_default_yes_contains_yes_hidden_yes_inc_hidden_no_check() {
  JsonnetInput::eval_error(
    r#"
local obj = { assert false : "bad", a:: 1 };
std.get(obj, "a", 2, inc_hidden=true)
"#,
    "bad",
  )
  .check();
}
