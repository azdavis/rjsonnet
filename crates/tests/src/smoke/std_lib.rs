//! Tests for the standard library.

use crate::check::JsonnetInput;

#[test]
fn type_num() {
  JsonnetInput::string(
    r"
std.type(3)
",
    "number",
  )
  .check();
}
