//! Duplicates.

use crate::check::JsonnetInput;

#[test]
fn local() {
  JsonnetInput::pre_eval_error(
    r"
##    v diagnostic: unused variable: `x`
local x = 3, x = 3;
##           ^ diagnostic: duplicate variable: `x`
x
",
  )
  .check();
}

#[test]
fn local_diff_ty() {
  JsonnetInput::pre_eval_error(
    r#"
##    v diagnostic: unused variable: `x`
local x = 3, x = "hi";
##           ^ diagnostic: duplicate variable: `x`
x
"#,
  )
  .check();
}

#[test]
fn object_local() {
  JsonnetInput::pre_eval_error(
    r"
{
  local x = 3,
##      ^ diagnostic: unused variable: `x`
  local x = 3,
##      ^ diagnostic: duplicate variable: `x`
  a: x,
}
",
  )
  .check();
}

#[test]
fn field_pre_eval() {
  JsonnetInput::pre_eval_error(
    r"
{
  a: 1,
  a: 2,
##^ diagnostic: duplicate field: `a`
}
",
  )
  .check();
}

#[test]
fn field_eval() {
  JsonnetInput::eval_error(
    r#"
local a = "a";
{
  [a]: 1,
  [a]: 2,
}
"#,
    "duplicate field: `a`",
  )
  .check();
}

#[test]
fn param() {
  JsonnetInput::pre_eval_error(
    r"
##           v diagnostic: duplicate variable: `x`
local bad(x, x) = x;
##        ^ diagnostic: unused variable: `x`
  bad(1)
##^^^ diagnostic: missing argument: `x` with type: `any`
",
  )
  .check();
}
