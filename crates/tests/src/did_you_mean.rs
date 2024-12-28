//! Providing suggestions for likely misspellings.

use crate::check::JsonnetInput;

#[test]
fn std_field() {
  JsonnetInput::eval_error(
    r"
  std.assertEq(1 + 1, 2)
##^^^^^^^^^^^^ diagnostic: no such field: `assertEq`; did you mean `assertEqual`?
",
    "no such field: `assertEq`",
  )
  .check();
}
