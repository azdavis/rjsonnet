//! Providing suggestions for likely misspellings.

use crate::check::JsonnetInput;

#[test]
fn std_field() {
  JsonnetInput::eval_error(
    r"
  std.assertEq(1 + 1, 2)
##^^^^^^^^^^^^ diagnostic: no such field: `assertEq`; did you mean: `assertEqual`?
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
##^^^^^^^ diagnostic: no such field: `bab`; did you mean: `bar`?
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
##                     ^^^ diagnostic: undefined variable: `nil`; did you mean: `null`?
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
##  ^^^^^ diagnostic: undefined variable: `thing`; did you mean: `thingy`?
",
  )
  .check();
}
