//! Tests for special identifiers that are actually reserved words, aka keywords.

use crate::check::JsonnetInput;

#[test]
fn bool_true() {
  JsonnetInput::manifest_self("true").check_one();
}

#[test]
fn bool_false() {
  JsonnetInput::manifest_self("false").check_one();
}

#[test]
fn null() {
  JsonnetInput::manifest_self("null").check_one();
}

#[test]
#[should_panic = "expected an identifier"]
fn redefine() {
  JsonnetInput::manifest(
    r"
local true = 1;
2
",
    "0",
  )
  .check_one();
}
