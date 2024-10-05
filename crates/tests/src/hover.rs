//! Testing that hovering contains certain info.

use crate::check::JsonnetInput;

#[test]
fn arr_num() {
  JsonnetInput::manifest(
    r"
[1, 2, 3]
##      ^ hover: number[]
",
    r"
[1, 2, 3]
",
  )
  .check();
}
