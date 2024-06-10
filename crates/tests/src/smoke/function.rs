//! Tests for functions.

// TODO remove the should panic and also fix some of these tests. some of the should panics are
// 'right', but these tests should be able to test BOTH pre-eval error AND eval error maybe? or one
// at a time (turn off optional static checks)?

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

#[test]
fn args_named_then_positional() {
  JsonnetInput::pre_eval_error(
    r"
local sub(x, y) = x - y;
  sub(
##^^^ diagnostic: missing argument: `y` with type: `number`
    x=9,
##    ^ diagnostic: extra named argument: `x`
    3,
##  ^^ diagnostic: positional arguments must not appear after named arguments
  )
",
  )
  .check();
}

#[test]
#[should_panic = "extra positional argument: 2"]
fn args_positional_extra() {
  JsonnetInput::eval_error(
    r"
local sub(x, y) = x - y;
  sub(3, 4, 5)
",
    "too many arguments; parameters (2): x, y; positional arguments: 3; named arguments: <none>",
  )
  .check();
}

#[test]
#[should_panic = "extra named argument: `z`"]
fn args_named_extra() {
  JsonnetInput::eval_error(
    r"
local sub(x, y) = x - y;
  sub(x=1, y=2, z=3)
",
    "too many arguments; parameters (2): x, y; positional arguments: <none>; named arguments (3): x, y, z",
  )
  .check();
}

#[test]
#[should_panic = "missing argument: `y`"]
fn args_positional_missing() {
  JsonnetInput::eval_error(
    r"
local sub(x, y) = x - y;
  sub(1)
",
    "parameter `y` was not defined at the function call site",
  )
  .check();
}

#[test]
#[should_panic = "missing argument: `y`"]
fn args_named_missing_1() {
  JsonnetInput::eval_error(
    r"
local sub(x, y) = x - y;
  sub(x=1)
",
    "parameter `y` was not defined at the function call site",
  )
  .check();
}

#[test]
#[should_panic = "missing argument: `x`"]
fn args_named_missing_2() {
  JsonnetInput::eval_error(
    r"
local sub(x, y) = x - y;
  sub(y=1)
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
#[should_panic = "extra named argument: `x`"]
fn args_named_positional_duplicate() {
  JsonnetInput::eval_error(
    r"
local sub(x, y) = x - y;
  sub(1, y=2, x=3)
",
    "too many arguments; parameters (2): x, y; positional arguments: 1; named arguments (2): y, x",
  )
  .check();
}

/// TODO fix. this is actually supposed to be allowed (wild)
#[test]
#[should_panic = "not in scope: `a`"]
fn default_arg_is_other_arg() {
  JsonnetInput::manifest(
    r"
local hm(a, b=a) = a + b;
hm(5)
",
    "10",
  )
  .check();
}

/// no surprise here.
#[test]
fn optional_after_required() {
  JsonnetInput::manifest(
    r"
local hm(a, b=1) = a + b;
hm(2) + hm(3, 4)
",
    "10",
  )
  .check();
}

/// but actually, this is allowed too.
#[test]
fn required_after_optional() {
  JsonnetInput::manifest(
    r"
local hm(a=1, b) = a + b;
hm(b=5) + hm(3, 4)
",
    "13",
  )
  .check();
}
