//! Duplicates.

use crate::check::JsonnetInput;

/// TODO: error range isn't the best.
#[test]
fn local() {
  JsonnetInput::pre_eval_error(
    r"
##    v diagnostic: unused: `x`
local x = 3, x = 3;
##               ^ diagnostic: duplicate binding: `x`
x
",
  )
  .check();
}

#[test]
fn object_local() {
  JsonnetInput::pre_eval_error(
    r"
{
  local x = 3,
##      ^ diagnostic: unused: `x`
  local x = 3,
##          ^ diagnostic: duplicate binding: `x`
  a: x,
}
",
  )
  .check();
}

#[test]
fn field() {
  JsonnetInput::pre_eval_error(
    r"
{
  a: 1,
  a: 2,
##^ diagnostic: duplicate field name: `a`
}
",
  )
  .check();
}

/// TODO: error range isn't the best.
#[test]
fn param() {
  JsonnetInput::pre_eval_error(
    r"
##       vvvvvv diagnostic: duplicate binding: `x`
local bad(x, x) = x;
##       ^^^^^^ diagnostic: unused: `x`
bad(1)
",
  )
  .check();
}
