//! Tests for comments in their various forms: `//`, `#`, and `/* */`.

use crate::check::JsonnetInput;

#[test]
fn slash_slash() {
  JsonnetInput::manifest(
    r"
1 + // 2 +
3
",
    "4.0",
  )
  .check();
}

#[test]
fn hash() {
  JsonnetInput::manifest(
    r"
1 # + 2
",
    "1.0",
  )
  .check();
}

#[test]
fn slash_star() {
  JsonnetInput::manifest(
    r"
2 /* * 2 */ + 3
",
    "5.0",
  )
  .check();
}
