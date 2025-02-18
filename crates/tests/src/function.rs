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

#[test]
fn args_named_then_positional() {
  JsonnetInput::pre_eval_error(
    r"
local sub(x, y) = x - y;
  sub(
##^^^ err: missing argument: `y` with type: `any`
    x=9,
##    ^ err: extra named argument: `x`
    3,
##  ^^ err: positional arguments must not appear after named arguments
  )
",
  )
  .check();
}

#[test]
fn args_positional_extra() {
  JsonnetInput::eval_error(
    r"
local sub(x, y) = x - y;
  sub(3, 4, 5)
##          ^ err: extra positional argument: 3
",
    "too many arguments",
  )
  .check();
}

#[test]
fn args_named_extra() {
  JsonnetInput::eval_error(
    r"
local sub(x, y) = x - y;
  sub(x=1, y=2, z=3)
##                ^ err: extra named argument: `z`
",
    "too many arguments",
  )
  .check();
}

#[test]
fn args_positional_missing() {
  JsonnetInput::eval_error(
    r"
local sub(x, y) = x - y;
  sub(1)
##^^^ err: missing argument: `y` with type: `any`
",
    "`y` was not defined",
  )
  .check();
}

#[test]
fn args_named_missing_1() {
  JsonnetInput::eval_error(
    r"
local sub(x, y) =
  assert std.isNumber(x);
  assert std.isNumber(y);
  x - y;

  sub(x=1)
##^^^ err: missing argument: `y` with type: `number`
",
    "`y` was not defined",
  )
  .check();
}

#[test]
fn args_named_missing_2() {
  JsonnetInput::eval_error(
    r"
local sub(x, y) =
  assert std.isNumber(x);
  assert std.isNumber(y);
  x - y;

  sub(y=1)
##^^^ err: missing argument: `x` with type: `number`
",
    "`x` was not defined",
  )
  .check();
}

#[test]
fn args_named_duplicate() {
  JsonnetInput::pre_eval_error(
    r"
local sub(x, y) = x - y;
  sub(x=1, x=2, y=3)
##           ^ err: duplicate named argument: `x`
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
##              ^ err: extra named argument: `x`
",
    "too many arguments",
  )
  .check();
}

#[test]
fn default_arg_is_other_given_arg() {
  JsonnetInput::manifest(
    r"
local f(a, b=a) = a + b;
f(5)
",
    "10",
  )
  .check();
}

#[test]
fn default_arg_is_other_default_arg() {
  JsonnetInput::manifest(
    r"
local f(a=2, b=a) = a + b;
f()
",
    "4",
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

#[test]
fn mutual_recur() {
  JsonnetInput::manifest(
    r#"
local
  isOdd(x) =
    if x == 0 then false
    else x == 1 || isEven(x - 1)
, isEven(x) =
    assert x >= 0 : "cannot figure out negative numbers";
    x == 0 || isOdd(x - 1)
;

isOdd(4)
"#,
    "false",
  )
  .check();
}

#[test]
fn ty_check_default() {
  JsonnetInput::manifest(
    r#"
local f(x=null) =
##        ^^^^ err: incompatible types; expected `number`; found `null`
  assert std.isNumber(x);
  x;

f(3)
"#,
    "3",
  )
  .check();
}
