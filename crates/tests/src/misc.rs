//! Miscellaneous tests.
//!
//! If you're not sure where a test should go, put it here.

use crate::check::JsonnetInput;

#[test]
fn func_arg_id_not_named_arg() {
  JsonnetInput::manifest(
    r"
local obj = { field: 3 };
local func(x, y) = if y == 4 then x;
func(obj.field, 4)
",
    "3",
  )
  .check();
}

/// this is a reduced case of a weird bug a while ago.
#[test]
fn same_fn_ty() {
  JsonnetInput::manifest(
    r#"
local foo() = null;
##    ^^^ err: unused variable: `foo`
local bar() = 1;
##    ^^^ err: unused variable: `bar`
local quz() = 2;

quz()
"#,
    "2",
  )
  .check();
}

#[test]
fn invalid_subscript() {
  JsonnetInput::eval_error(
    r#"
  null[1]
##^^^^ err: invalid subscript; expected a type with fields or elements; found `null`
"#,
    "incompatible types",
  )
  .check();
}

#[test]
fn invalid_super() {
  JsonnetInput::pre_eval_error(
    r#"
{ c: 3 } + {
  a: 1,
  b:
    local guy =
      if std.extVar("x") == "1" then
        super
##      ^^^^^ err: `super` must be used with `.`, `[]`, or `in`
      else
        {};
    "c" in guy
}
"#,
  )
  .check();
}

#[test]
fn hole() {
  JsonnetInput::pre_eval_error(
    r#"
local f(x) = ... - x;
##           ^^^ err: found placeholder hole
f(3) + 4
"#,
  )
  .check();
}

#[test]
fn tailstrict() {
  JsonnetInput::pre_eval_error(
    r#"
local z(n) =
  if n == 0 then 0
  else z(n - 1) tailstrict;
##              ^^^^^^^^^^ err: `tailstrict` is unstable
z(4)
"#,
  )
  .check();
}

#[test]
fn subscript_str() {
  JsonnetInput::manifest(
    r#"
{
  local a = "hello"[3],
##      ^ type: string
  local k = "新幹線"[1],
##      ^ type: string
  "ascii": a,
  "kanji": k,
}
"#,
    r#"
{
  "ascii": "l",
  "kanji": "幹"
}
"#,
  )
  .check();
}

#[test]
fn call_non_fn() {
  JsonnetInput::eval_error(
    r#"
  null()
##^^^^^^ err: invalid call; expected a callable type; found `null`
"#,
    "incompatible types",
  )
  .check();
}

#[test]
fn tuple_too_few() {
  JsonnetInput::manifest_or_fn(
    r#"
function()
  local f(xs) = assert std.isArray(xs) && std.length(xs) == 3; xs[0];
  f([1, 2])
##  ^^^^^^ err: wrong number of tuple elements; expected 3; found 2
  "#,
  )
  .check();
}

#[test]
fn tuple_too_many() {
  JsonnetInput::manifest_or_fn(
    r#"
function()
  local f(xs) = assert std.isArray(xs) && std.length(xs) == 3; xs[0];
  f([1, 2, 3, 4, 5])
##  ^^^^^^^^^^^^^^^ err: wrong number of tuple elements; expected 3; found 5
  "#,
  )
  .check();
}
