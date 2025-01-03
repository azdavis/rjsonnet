//! Tests for unreachable code, which is an expression that contains a sub-expression of type
//! `never` that definitely needs to be evaluated to evaluate the larger expression.

use crate::check::JsonnetInput;

#[test]
fn subscript() {
  JsonnetInput::eval_error(
    r#"
  (error "no").foo
##^^^^^^^^^^^^^^^^ diagnostic: unreachable code
"#,
    "no",
  )
  .check();
}

#[test]
fn if_cond() {
  JsonnetInput::eval_error(
    r#"
  if error "no" then 3
##^^^^^^^^^^^^^^^^^^^^ diagnostic: unreachable code
"#,
    "no",
  )
  .check();
}

#[test]
fn through_local() {
  JsonnetInput::eval_error(
    r#"
local a = error "a";
local b = 1 + 2;
  a - b
##^^^^^ diagnostic: unreachable code
"#,
    "a",
  )
  .check();
}

#[test]
fn error() {
  JsonnetInput::eval_error(
    r#"
  error error "no"
##^^^^^^^^^^^^^^^^ diagnostic: unreachable code
"#,
    "no",
  )
  .check();
}
