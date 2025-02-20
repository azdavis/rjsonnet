//! Tests for object asserts with regular field gets with `.` or `[]`.

use crate::check::JsonnetInput;

#[test]
fn yes() {
  JsonnetInput::eval_error(
    r#"
local obj = { assert false : "bad", a: 1 };
obj.a
"#,
    "bad",
  )
  .check();
}

#[test]
fn idx_err() {
  JsonnetInput::eval_error(
    r#"
local obj = { assert false : "object assert", a: error "value" };
  obj[error "field"]
##^^^^^^^^^^^^^^^^^^ err: unreachable code
"#,
    "field",
  )
  .check();
}

#[test]
fn object_assert_err() {
  JsonnetInput::eval_error(
    r#"
local obj = { assert false : "object assert", a: error "value" };
obj.a
"#,
    "object assert",
  )
  .check();
}

#[test]
fn value_err() {
  JsonnetInput::eval_error(
    r#"
local obj = { a: error "value" };
obj.a
"#,
    "value",
  )
  .check();
}

#[test]
fn no_such_field() {
  JsonnetInput::eval_error(
    r#"
local obj = { assert false : "object assert", a: error "value" };
// prevent statically knowing the fields
local id(x) = x;
id(obj).b
"#,
    "object assert",
  )
  .check();
}
