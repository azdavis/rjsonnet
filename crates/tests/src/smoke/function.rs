//! Tests for functions.

use crate::check::JsonnetInput;

#[test]
fn function() {
  JsonnetInput::manifest(
    r"
local inc = function(x) x + 1;
inc(3)
",
    "4",
  )
  .check();
}

#[test]
fn local_sugar() {
  JsonnetInput::manifest(
    r"
local dec(x) = x - 1;
dec(3)
",
    "2",
  )
  .check();
}

#[test]
fn args_positional() {
  JsonnetInput::manifest(
    r"
local mul(x, y) = x * y;
mul(2, 3)
",
    "6",
  )
  .check();
}

#[test]
fn args_named_in_order() {
  JsonnetInput::manifest(
    r"
local mul(x, y) = x * y;
mul(x=4, y=5)
",
    "20",
  )
  .check();
}

#[test]
fn args_named_out_of_order() {
  JsonnetInput::manifest(
    r"
local div(x, y) = x / y;
div(y=3, x=9)
",
    "3",
  )
  .check();
}

#[test]
fn args_positional_then_named() {
  JsonnetInput::manifest(
    r"
local sub(x, y) = x - y;
sub(8, y=9)
",
    "-1",
  )
  .check();
}

// TODO should emit error about named then positional arg
#[test]
#[should_panic = "duplicate argument: x"]
fn args_named_then_positional() {
  JsonnetInput::pre_eval_error(
    r"
local sub(x, y) = x - y;
  sub(x=9, 3)
",
  )
  .check();
}

#[test]
fn args_positional_extra() {
  JsonnetInput::eval_error(
    r"
local sub(x, y) = x - y;
  sub(1, 2, 3)
##^^^^^^^^^^^^ diagnostic: <eval>
",
    "too many arguments; parameters (2): x, y; positional arguments: 3; named arguments: <none>",
  )
  .check();
}

#[test]
fn args_named_extra() {
  JsonnetInput::eval_error(
    r"
local sub(x, y) = x - y;
  sub(x=1, y=2, z=3)
##^^^^^^^^^^^^^^^^^^ diagnostic: <eval>
",
    "too many arguments; parameters (2): x, y; positional arguments: <none>; named arguments (3): x, y, z",
  )
  .check();
}

#[test]
fn args_positional_missing() {
  JsonnetInput::eval_error(
    r"
local sub(x, y) = x - y;
  sub(1)
##^^^^^^ diagnostic: <eval>
",
    "parameter `y` was not defined at the function call site",
  )
  .check();
}

#[test]
fn args_named_missing_1() {
  JsonnetInput::eval_error(
    r"
local sub(x, y) = x - y;
  sub(x=1)
##^^^^^^^^ diagnostic: <eval>
",
    "parameter `y` was not defined at the function call site",
  )
  .check();
}

#[test]
fn args_named_missing_2() {
  JsonnetInput::eval_error(
    r"
local sub(x, y) = x - y;
  sub(y=1)
##^^^^^^^^ diagnostic: <eval>
",
    "parameter `x` was not defined at the function call site",
  )
  .check();
}

#[test]
fn args_named_duplicate() {
  JsonnetInput::pre_eval_error(
    r"
local sub(x, y) = x - y;
  sub(x=1, x=2, y=3)
##           ^ diagnostic: duplicate named argument: `x`
",
  )
  .check();
}

#[test]
fn args_named_positional_duplicate() {
  JsonnetInput::eval_error(
    r"
local sub(x, y) = x - y;
  sub(1, y=2, x=3)
##^^^^^^^^^^^^^^^^ diagnostic: <eval>
",
    "too many arguments; parameters (2): x, y; positional arguments: 1; named arguments (2): y, x",
  )
  .check();
}
