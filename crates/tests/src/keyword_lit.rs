//! Tests for special identifiers that are actually reserved words, aka keywords.

use crate::check::JsonnetInput;

#[test]
fn bool_true() {
  JsonnetInput::manifest_self("true").check();
}

#[test]
fn bool_false() {
  JsonnetInput::manifest_self("false").check();
}

#[test]
fn null() {
  JsonnetInput::manifest_self("null").check();
}

#[test]
fn redefine() {
  JsonnetInput::pre_eval_error(
    r"
##         v err: trailing token
local true = 1;
##    ^^^^ err: expected `;`, found `true`
2
",
  )
  .check();
}
