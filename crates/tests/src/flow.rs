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

#[test]
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

#[test]
fn not_2() {
  JsonnetInput::manifest(
    r#"
local f(x) =
##    ^ hover: (x: null | number) => number
  assert x == null || std.isNumber(x);
  if x != null then
##   ^ hover: null | number
    x
##  ^ hover: number
  else
    assert x == null;
##         ^ hover: null | number
#            ... not great, but `if !c then a else b` is bad style anyway.
    assert x == null;
##         ^ hover: null
    0;

[f(null), f(3)]
"#,
    r#"
[0, 3]
"#,
  )
  .check();
}
