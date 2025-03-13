//! Tests focusing on flow typing.

use crate::check::JsonnetInput;

#[test]
fn smoke() {
  JsonnetInput::manifest(
    r#"
local f(x) =
##    ^ hover: (x: number | string) => number
  assert std.isNumber(x) || std.isString(x);
  if std.isNumber(x) then
##                ^ hover: number | string
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
##vvvvv err: invalid use of `+`; expected addable types; left: `function`; right: `number`
  f + 1
# ^ hover: function
;

[
  bad(3),
##    ^ err: incompatible types; expected `function`; found `number`
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
##             ^^^ hover: { foo: never, ... }

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
      assert !std.isBoolean(obj.a) && obj.a != null;
      std.length(obj.a)
##               ^^^ hover: { a: string | array[any] | object | function, ... }
  else
    std.length(obj)
##             ^^^ hover: { a: never, ... }
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
##  ^ type: { foo: never, bar: never, ... }
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
##  ^ type: { foo: never, ... }
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
##      ^ type: { a: string, b: boolean | null | string | array[any] | object | function, ... } | { a: string, b: never, ... }
    else
##      v completions: a: boolean | null | number | array[any] | object | function
      x.a
##    ^ type: { a: boolean | null | number | array[any] | object | function, ... } | { a: never, ... }
  else
    x
##  ^ type: boolean | null | number | string | array[any] | function
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
##    ^ type: function
  else
    x
##  ^ type: boolean | null | number | string | array[any] | object
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

#[test]
fn partial_1() {
  JsonnetInput::manifest(
    r#"
local f(x) =
  if std.isDecimal(x) then
    x + 0.5
##  ^ type: number
  else if std.isInteger(x) then
    x + 1
##  ^ type: number
;

local n = f(5);
##        ^ type: (x: any) => null | number

if n == null then 5 else n + 2
##                       ^ type: number
"#,
    "8",
  )
  .check();
}

#[test]
fn partial_2() {
  JsonnetInput::manifest(
    r#"
local f(x) =
  assert std.isString(x);
  if x == "hi" then
    "hey"
  else if x == "bye" then
    "see ya"
  else
    x
##  ^ type: string
;

local s = f("hello");
##        ^ type: (x: string) => string

std.length(s)
"#,
    "5",
  )
  .check();
}

#[test]
fn partial_3() {
  JsonnetInput::manifest(
    r#"
local f(x) =
  if x == "hi" then
    std.length(x)
##             ^ type: string
  else if x == "bye" then
    std.length(x)
##             ^ type: string
  else
    x
##  ^ type: any
;

local s = f("hello");
##        ^ type: (x: any) => any

std.length(s)
"#,
    "5",
  )
  .check();
}

#[test]
fn fn_len_assert() {
  JsonnetInput::manifest_or_fn(
    r#"
function(f)
  assert std.isFunction(f);
  assert std.length(f) == 2;
  f
##^ type: ($a: any, $b: any) => any
"#,
  )
  .check();
}

#[test]
fn filter() {
  JsonnetInput::manifest_or_fn(
    r#"
function(xs)
  assert std.isArray(xs);
  assert std.all(std.map(function(x) std.isNumber(x) || std.isString(x), xs));
  local ys = std.filter(std.isNumber, xs);
##                                    ^^ type: array[number | string]
  ys
##^^ type: array[number]
"#,
  )
  .check();
}

#[test]
fn filter_map() {
  JsonnetInput::manifest_or_fn(
    r#"
local inc(x) =
##    ^^^ type: (x: number) => number
  assert std.isNumber(x);
  x + 1;

function(xs)
  assert std.isArray(xs);
  assert std.all(std.map(function(x) std.isNumber(x) || std.isString(x), xs));
##      vv type: array[number]
  local ys = std.filterMap(std.isNumber, inc, xs);
##                                            ^^ type: array[number | string]
  std.filterMap(function(x) x != 1, inc, xs) + ys
##                                       ^^ err: incompatible types; expected `number`; found `string`
"#,
  )
  .check();
}

#[test]
fn obj_field() {
  JsonnetInput::manifest_or_fn(
    r#"
function(x)
  assert std.isObject(x) && std.isString(x.t);
  if x.t == "foo" then
    1
  else if x.t == "bar" then
    2
  else if x.t == "quz" then
    3
  else
    4
"#,
  )
  .check();
}

#[test]
#[should_panic = "none of the lines were equal"]
fn conditional_object_field() {
  JsonnetInput::manifest_or_fn(
    r#"
function(x)
  assert x == null || std.isString(x);
  {
    hi: "there",
    [if x != null then "x_len"]: std.length(x),
##                                          ^ type: string
  }
"#,
  )
  .check();
}

/// NOTE array[never] is not great
#[test]
fn conditional_comprehension() {
  JsonnetInput::manifest_or_fn(
    r#"
function(xs)
  assert std.isArray(xs);
  assert std.all(std.map(function(x) x == null || std.isNumber(x), xs));
  local zs = [x + 1 for x in xs if x != null];
##            ^ type: number
##                                         v type: number
  local ys = [(if x == null then "no" else x - 1) for x in xs];
##                ^ type: null | number
##      vv type: array[number]
  { zs: zs, ys: ys }
##              ^^ type: array[number | string]
"#,
  )
  .check();
}
