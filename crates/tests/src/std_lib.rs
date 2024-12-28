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

// TODO need to lazily evaluate std function arguments just like regular function arguments
#[test]
#[should_panic = "not yet implemented: get"]
fn get() {
  JsonnetInput::manifest(
    r#"
std.get({a: 1}, "a", error "no")
"#,
    "1",
  )
  .check();
}
