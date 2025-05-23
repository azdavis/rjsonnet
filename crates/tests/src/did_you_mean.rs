//! Providing suggestions for likely misspellings.

use crate::check::JsonnetInput;

#[test]
fn std_field() {
  JsonnetInput::eval_error(
    r"
  std.assertEq(1 + 1, 2)
##^^^^^^^^^^^^ err: no such field: `assertEq`; did you mean: `assertEqual`?
",
    "no such field: `assertEq`",
  )
  .check();
}

#[test]
fn user_written_field() {
  JsonnetInput::eval_error(
    r"
local obj = {
  foo: 1,
  bar: 3,
  quz: 6
};

  obj.bab
##^^^^^^^ err: no such field: `bab`; did you mean: `bar`?
",
    "no such field: `bab`",
  )
  .check();
}

#[test]
fn exact_id() {
  JsonnetInput::pre_eval_error(
    r"
local f(x) = if x then nil else 4;
##                     ^^^ err: undefined variable: `nil`; did you mean: `null`?
f(false)
",
  )
  .check();
}

#[test]
fn approx_id() {
  JsonnetInput::pre_eval_error(
    r"
local thingy = 4;
3 + thing + thingy
##  ^^^^^ err: undefined variable: `thing`; did you mean: `thingy`?
",
  )
  .check();
}

#[test]
#[should_panic = "undefined variable: `foo`; did you mean: `foo`?"]
fn not_in_scope_same() {
  JsonnetInput::pre_eval_error(
    r#"
local f(foo) = foo + 1;
local g() = foo + 2;
##          ^^^ err: undefined variable: `foo`
f(3) + g()
  "#,
  )
  .check();
}

#[test]
#[should_panic = "undefined variable: `goo`; did you mean: `foo`?"]
fn not_in_scope_different() {
  JsonnetInput::pre_eval_error(
    r#"
local f(foo) = foo + 1;
local g(bar) = goo + 2;
##             ^^^ err: undefined variable: `goo`
f(3) + g(4)
  "#,
  )
  .check();
}
