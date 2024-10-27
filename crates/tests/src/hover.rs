//! Testing that hovering contains certain info.

use crate::check::JsonnetInput;

#[test]
fn arr_num() {
  JsonnetInput::manifest(
    r"
[1, 2, 3]
##      ^ hover: number[]
",
    r"
[1, 2, 3]
",
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
fn foldl() {
  JsonnetInput::eval_error(
    r"
local objAdd(a, b) =
  assert std.isObject(a);
  assert std.isObject(b);
  a + b;

local result = std.foldl(objAdd, [{a: 1}, {b: 2}, {c: 3}], {d: 4});
result
## ^ hover: { ... } | { d: number }
",
    "not yet implemented: std.foldl",
  )
  .check();
}

#[test]
fn assert_or() {
  JsonnetInput::manifest(
    r"
local thing(a, b) =
##    ^ hover: (a: null | number, b: true | false | string) => number
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
fn flow() {
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
