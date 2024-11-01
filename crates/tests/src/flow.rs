//! Tests focusing on flow typing.

use crate::check::JsonnetInput;

#[test]
fn smoke() {
  JsonnetInput::manifest(
    r#"
local f(x) =
##    ^ hover: (x: string | number) => number
  assert std.isNumber(x) || std.isString(x);
  if std.isNumber(x) then
##                ^ hover: string | number
    x + 1
##  ^ hover: number
  else
    std.length(x);
##             ^ hover: string

f(3) + f("hi")
"#,
    "6",
  )
  .check();
}

// TODO fix, need to get info from !=
#[test]
#[should_panic = "none of the lines were equal"]
fn not() {
  JsonnetInput::manifest(
    r#"
local f(x) =
##    ^ hover: (x: null | string) => string
  assert x == null || std.isString(x);
  if x != null && std.length(x) >= 10 then
    x
  else
    "Hi";

f(null)
"#,
    r#"
"Hi"
"#,
  )
  .check();
}
