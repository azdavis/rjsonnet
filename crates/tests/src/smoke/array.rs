//! Tests for arrays, aka lists, aka the stuff surrounded by `[]`.

use crate::check::JsonnetInput;

#[test]
fn empty() {
  JsonnetInput::manifest_self("[]").check();
}

#[test]
fn non_empty() {
  JsonnetInput::manifest_self(
    r#"
[1, true, "foo"]
"#,
  )
  .check();
}

#[test]
fn plus() {
  JsonnetInput::manifest(
    r"
[1, 3] + [2, 4]
",
    r"
[1, 3, 2, 4]
",
  )
  .check();
}
