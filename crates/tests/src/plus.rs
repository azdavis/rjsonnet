//! Tests for `+`-related things.

use crate::check::JsonnetInput;

#[test]
fn arrays_ifs() {
  JsonnetInput::manifest(
    r#"
local f(x) =
  assert std.isBoolean(x);
  if x then [1, 2] else [];

f(true) + f(false)
"#,
    r#"[1, 2]"#,
  )
  .check();
}

#[test]
fn any_to_non_addable() {
  JsonnetInput::manifest_or_fn(
    r#"
function(y)
  y + null
##^^^^^^^^ err: invalid use of `+`; expected addable types; left: `any`; right: `null`
"#,
  )
  .check();
}

#[test]
fn tuple() {
  JsonnetInput::manifest(
    r#"
local a = [1, "hi"];
##    ^ type: tuple[number, string]
local b = [null, false];
##    ^ type: tuple[null, false]
local c = a + b;
##    ^ type: tuple[number, string, null, false]
c
"#,
    r#"
[1, "hi", null, false]
"#,
  )
  .check();
}
