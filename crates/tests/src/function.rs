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
##^^^ diagnostic: missing argument: `y` with type: `any`
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
fn args_positional_extra() {
  JsonnetInput::eval_error(
    r"
local sub(x, y) = x - y;
  sub(3, 4, 5)
##          ^ diagnostic: extra positional argument: 3
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
##                ^ diagnostic: extra named argument: `z`
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
##^^^ diagnostic: missing argument: `y` with type: `any`
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
##^^^ diagnostic: missing argument: `y` with type: `number`
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
##^^^ diagnostic: missing argument: `x` with type: `number`
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
##              ^ diagnostic: extra named argument: `x`
",
    "too many arguments",
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

// TODO fix this bug (something with mutual recursion in the envs)
#[test]
fn mutual_recur() {
  JsonnetInput::eval_error(
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
    "not in scope: `isEven`",
  )
  .check();
}

#[test]
#[should_panic = "no diagnostics at range"]
fn ty_check_default() {
  JsonnetInput::manifest(
    r#"
local f(a=null) =
##        ^^^^ diagnostic: incompatible types; expected `number`; found `null`
  assert std.isNumber(a);
  a;

f(3)
"#,
    "3",
  )
  .check();
}
