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

#[test]
fn obj_array_self() {
  JsonnetInput::manifest(
    r#"
{
  foo: [
    { kind: 'Soda', qty: 2 },
  ],
  quz: self.foo,
}
"#,
    r#"
{
  "foo": [{ "kind": "Soda", "qty": 2 }],
  "quz": [{ "kind": "Soda", "qty": 2 }]
}
"#,
  )
  .check();
}

// this is a reduced case of a weird bug a while ago.
#[test]
fn same_fn_ty() {
  JsonnetInput::manifest(
    r#"
local foo() = null;
##    ^^^ diagnostic: unused variable: `foo`
local bar() = 1;
##    ^^^ diagnostic: unused variable: `bar`
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
##^^^^ diagnostic: invalid subscript, i.e. use of `[]` or `.`; expected a type with fields or elements, e.g. `array[any]`, `object`; found `null`
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
##      ^^^^^ diagnostic: `super` must be used with `.`, `[]`, or `in`
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
##           ^^^ diagnostic: found placeholder hole
f(3) + 4
"#,
  )
  .check();
}

#[test]
fn add_arrays_ifs() {
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
fn add_any_to_non_addable() {
  JsonnetInput::manifest_or_fn(
    r#"
function(y)
  y + null
##^^^^^^^^ diagnostic: invalid use of `+`; expected addable types, e.g. `number`, `string`, `object`, `array[any]`; left: `any`; right: `null`
"#,
  )
  .check();
}
