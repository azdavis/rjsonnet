//! Duplicates.

use crate::check::JsonnetInput;

/// TODO: error range isn't the best.
#[test]
fn local() {
  JsonnetInput::pre_eval_error(
    r"
##    v diagnostic: unused: `x`
local x = 3, x = 4;
##               ^ diagnostic: duplicate binding: `x`
x
",
  )
  .check();
}

// TODO fix
#[test]
#[should_panic = "no diagnostic matches: duplicate binding: `x`"]
fn object_local() {
  JsonnetInput::pre_eval_error(
    r"
{
  local x = 3,
  local x = 4,
##      ^ diagnostic: duplicate binding: `x`
  a: 1,
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
