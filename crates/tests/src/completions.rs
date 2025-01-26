//! Tests for completions.

use crate::check::JsonnetInput;

#[test]
fn smoke() {
  JsonnetInput::manifest_or_fn(
    r#"
local obj = { a: 1, b: null, c: "hi" };
obj.a
##  ^ completions: a: number; b: null; c: string
"#,
  )
  .check();
}

#[test]
fn union_inside() {
  JsonnetInput::manifest_or_fn(
    r#"
function(b)
  assert std.isBoolean(b);
  local obj = { a: if b then 1 else "hi" };
  obj.a
##    ^ completions: a: number | string
"#,
  )
  .check();
}

#[test]
fn union_outside() {
  JsonnetInput::manifest_or_fn(
    r#"
function(b)
  assert std.isBoolean(b);
  local obj = if b then { a: 1 } else { a: "hi" };
  obj.a
##    ^ completions: a: number | string
"#,
  )
  .check();
}
