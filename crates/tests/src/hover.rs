//! Testing that hovering contains certain info.

use crate::check::JsonnetInput;

#[test]
fn tup_num() {
  JsonnetInput::manifest(
    r#"
[1, "hi", false]
##             ^ hover: tuple[number, string, false]
"#,
    r#"
[1, "hi", false]
"#,
  )
  .check();
}

#[test]
fn func_str_null() {
  JsonnetInput::manifest(
    r#"
local f(x) =
  if x == 1 then
    "a"
  else if x == 2 then
    "b"
  else
    null;

local xs = [null, f];
##                ^ hover: (x: any) => null | string

xs[0]
"#,
    "null",
  )
  .check();
}

#[test]
fn assert_param_ty_1() {
  JsonnetInput::manifest(
    r"
local addOne(foo) =
  assert std.isNumber(foo);
  foo + 1;

addOne(3)
## ^ hover: (foo: number) => number
",
    "4",
  )
  .check();
}

#[test]
fn assert_param_type_2() {
  JsonnetInput::manifest(
    r"
local greet(name) =
  assert std.type(name) == 'string';
  'hello, ' + name + '!';

greet('fella')
## ^ hover: (name: string) => string
",
    r#"
"hello, fella!"
"#,
  )
  .check();
}

#[test]
fn assert_param_type_3() {
  JsonnetInput::manifest(
    r"
local maybe(a, b) =
  assert std.isBoolean(a) && std.isString(b);
  if a then b;

maybe(false, 'hey')
## ^ hover: (a: boolean, b: string) => null | string
",
    "null",
  )
  .check();
}

#[test]
fn local_fn_ty() {
  JsonnetInput::manifest(
    r"
local mkNull() = null;
##    ^ hover: () => null
mkNull()
",
    "null",
  )
  .check();
}

#[test]
#[should_panic = "not yet implemented: foldl"]
fn foldl() {
  JsonnetInput::manifest(
    r"
local objAdd(a, b) =
  assert std.isObject(a);
  assert std.isObject(b);
  a + b;

local result = std.foldl(objAdd, [{a: 1}, {b: 2}, {c: 3}], {d: 4});
result
## ^ hover: object | { d: number }
",
    r#"
{
  "a": 1,
  "b": 2,
  "c": 3,
  "d": 4
}
"#,
  )
  .check();
}

#[test]
fn assert_or() {
  JsonnetInput::manifest(
    r"
local thing(a, b) =
##    ^ hover: (a: null | number, b: boolean | string) => number
  assert std.isNumber(a) || a == null;
  assert std.isString(b) || std.isBoolean(b);
  if a == null then
    3
  else if std.isString(b) then
    4
  else
    6;

thing(null, false)
",
    "3",
  )
  .check();
}

#[test]
fn object_assert_1() {
  JsonnetInput::manifest(
    r#"
local func(x) = {
##    ^ hover: (x: any) => { thing: string }
  local thunk(x) =
    assert std.isNumber(x);
    'hi',
  thing: thunk(x),
};

func(1)
"#,
    r#"
{
  "thing": "hi"
}
"#,
  )
  .check();
}

#[test]
fn object_assert_2() {
  JsonnetInput::manifest(
    r#"
local func(x) = {
##    ^ hover: (x: number) => { thing: number }
  assert std.isNumber(x),
  thing: x + 1,
};

func(1)
"#,
    r#"
{
  "thing": 2
}
"#,
  )
  .check();
}

#[test]
fn object_assert_3() {
  JsonnetInput::manifest(
    r#"
##         v err: unused variable: `x`
local func(x) = {
##    ^ hover: (x: any) => { thing: number }
  local x = 123,
  assert std.isNumber(x),
  thing: x + 1,
};

func(3)
"#,
    r#"
{
  "thing": 124
}
"#,
  )
  .check();
}

#[test]
fn eq_lit_assert() {
  JsonnetInput::manifest(
    r#"
local f(x) =
##    ^ hover: (x: string) => number
  assert std.isString(x);
  if x == "hi" then
    1
  else if x == "bye" then
    2
  else
    std.length(x)
##             ^ hover: string
;

f("bye")
"#,
    "2",
  )
  .check();
}

#[test]
fn take_bool() {
  JsonnetInput::manifest(
    r#"
local f(x) =
##    ^ hover: (x: boolean) => number
  assert std.isBoolean(x);
  if x then 1 else 0
;

[f(true), f(false)]
"#,
    r#"
[1, 0]
"#,
  )
  .check();
}

#[test]
fn set() {
  JsonnetInput::manifest_or_fn(
    r"
local singleton(x) = std.set([x]);

function(x)
  assert std.isNumber(x);
##          v hover: (x: any) => set[any]
  local a = singleton(x);
##      ^ hover: set[any]
  local b = std.set([x]);
##      ^ hover: set[number]
  {a: a, b: b }
",
  )
  .check();
}

#[test]
fn non_ident_field() {
  JsonnetInput::manifest(
    r#"
  { "foo bar": 3, "the-field": 4 }
##^ hover: { "foo bar": number, "the-field": number }
"#,
    r#"
{ "foo bar": 3, "the-field": 4 }
"#,
  )
  .check();
}

#[test]
fn escaped_field() {
  JsonnetInput::manifest(
    r#"
  { "do\nut\bar\test\"\false\\": 3 }
##^ hover: { "do\nut\bar\test\"\false\\": number }
"#,
    r#"
{ "do\nut\bar\test\"\false\\": 3 }
"#,
  )
  .check();
}

#[test]
fn param_function() {
  JsonnetInput::manifest_or_fn(
    r#"
function(x)
##       ^ hover: number
  assert std.isNumber(x);
  x + 1
"#,
  )
  .check();
}

#[test]
fn param_local() {
  JsonnetInput::manifest_or_fn(
    r#"
local f(x) =
##      ^ hover: number
  assert std.isNumber(x);
  x + 1
; f
"#,
  )
  .check();
}

#[test]
fn param_obj_field() {
  JsonnetInput::manifest_or_fn(
    r#"
{
  f(x):
##  ^ hover: number
    assert std.isNumber(x);
    x + 1
}
"#,
  )
  .check();
}

#[test]
fn return_obj() {
  JsonnetInput::manifest_or_fn(
    r#"
local f(x) = {
  assert std.isNumber(x),
  y: x + 1,
};
  f
##^ type: (x: number) => { y: number }
"#,
  )
  .check();
}

#[test]
#[should_panic = "none of the lines were equal"]
fn return_obj_as_obj_field() {
  JsonnetInput::manifest_or_fn(
    r#"
local obj = {
  f(x): {
    assert std.isNumber(x),
    y: x + 1,
  }
};
  obj
##^^^ type: { f: (x: number) => { y: number } }
"#,
  )
  .check();
}

#[test]
fn no_comment() {
  JsonnetInput::manifest(
    r#"
// hi there
## ^ hover: <none>
1 + 1
"#,
    "2",
  )
  .check();
}

#[test]
fn no_whitespace() {
  JsonnetInput::manifest(
    r#"
local      one = 1;
##     ^ hover: <none>
one + one
"#,
    "2",
  )
  .check();
}
