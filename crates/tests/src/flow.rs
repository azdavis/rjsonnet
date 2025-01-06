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
##         ^ hover: null
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

#[test]
fn not_and() {
  JsonnetInput::manifest(
    r#"
local f(x) =
  if std.isObject(x) && "foo" in x && !("foo" in x && "bar" in x) then
    x.foo
##    ^^^ type: any
;

f(null)
"#,
    "null",
  )
  .check();
}

#[test]
fn not_or_1() {
  JsonnetInput::manifest(
    r#"
local f(x) =
  if std.isObject(x) && "foo" in x && !("foo" in x || "bar" in x) then
    x
##  ^ type: never
;

f(null)
"#,
    "null",
  )
  .check();
}

#[test]
fn not_or_2() {
  JsonnetInput::manifest(
    r#"
local f(x) =
  if std.isObject(x) && "foo" in x && !("foo" in x || std.length(x) == 5) then
    x
##  ^ type: never
;

f(null)
"#,
    "null",
  )
  .check();
}

#[test]
fn len_obj_unknown() {
  JsonnetInput::manifest(
    r#"
local f(x) =
  if std.isObject(x) then
    if "a" in x && std.isString(x.a) then
      if "b" in x && std.isNumber(x.b) then
        if std.length(x) == 2 then
          std.length(x.a) + x.b
##                   ^ type: { a: string, b: number }
        else if std.length(x) == 1 then
          x
##        ^ type: never
        else if std.length(x) == 3 then
          x
##        ^ type: { a: string, b: number, ... }
        else
          x
##        ^ type: { a: string, b: number, ... }
      else
        x
##      ^ type: { a: string, ... }
    else
      x
##    ^ type: object
  else
    x
##  ^ type: any
;

f(null)
"#,
    "null",
  )
  .check();
}

#[test]
fn len_obj_known() {
  JsonnetInput::manifest(
    r#"
local x = { a: 1, b: "hi" };

if std.length(x) == 1 then
  x
##^ type: never
else if std.length(x) == 3 then
  x
##^ type: never
else if std.length(x) == 2 then
  x.a
##  ^ type: number
"#,
    "1",
  )
  .check();
}

#[test]
fn len_fn_unknown() {
  JsonnetInput::manifest(
    r#"
local f(x) =
  if std.isFunction(x) then
    if std.length(x) == 2 then
      x(3, 5)
##    ^ type: ($a: any, $b: any) => any
    else
      x
##    ^ type: (...) => any
  else
    x
##  ^ type: any
;

f(null)
"#,
    "null",
  )
  .check();
}

#[test]
fn len_fn_known() {
  JsonnetInput::manifest(
    r#"
local f(x, y) = if x then y;

if std.length(f) == 1 then
  f
##^ type: never
else if std.length(f) == 3 then
  f
##^ type: never
else if std.length(f) == 2 then
  f(true, 5)
"#,
    "5",
  )
  .check();
}
