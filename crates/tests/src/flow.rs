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

#[test]
fn is_fn() {
  let thing = r#"
local bad(f) =
  assert std.isFunction(f) : "not a fn";
##vvvvv diagnostic: not a pair of types that can be added with `+`; left: `(...) => any`; right: `number`
  f + 1
# ^ hover: (...) => any
;

[
  bad(3),
##    ^ diagnostic: incompatible types; expected `(...) => any`; found `number`
  bad(function() 3),
  bad(function(x) x + 3),
  bad(function(x, y) x + y),
  bad(function(x=1, y=2) x + y),
]
"#;
  JsonnetInput::eval_error(thing, "not a fn").check();
}

#[test]
fn obj_field_in() {
  JsonnetInput::manifest(
    r#"
local f(obj) =
  assert std.isObject(obj);
  if "foo" in obj then
##            ^^^ hover: object
    obj.foo
##  ^^^ hover: { foo: any, ... }
  else
    std.length(obj);
##             ^^^ hover: object

f({thing: 5}) + f({foo: 2})
"#,
    "3",
  )
  .check();
}

#[test]
fn obj_field_in_known() {
  JsonnetInput::manifest(
    r#"
local f(b) =
  local obj = if b then { foo: 3 } else {};
  if "foo" in obj then obj.foo else 4;
##                     ^^^ hover: { foo: number }

[f(true), f(false)]
"#,
    "[3, 4]",
  )
  .check();
}

#[test]
fn obj_field_ty() {
  JsonnetInput::manifest(
    r#"
local f(obj) =
  assert std.isObject(obj);
##                    ^^^ hover: object
  if "a" in obj then
    if std.isNumber(obj.a) then
##                  ^^^ hover: { a: any, ... }
      obj.a + 7
##    ^^^ hover: { a: number, ... }
    else
      std.length(obj.a)
##               ^^^ hover: { a: any, ... }
  else
    std.length(obj)
##             ^^^ hover: object
;

[f({b: null}), f({a: "hello"}), f({a: 4})]
"#,
    "[1, 5, 11]",
  )
  .check();
}
