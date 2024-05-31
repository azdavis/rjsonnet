//! Unused bindings.

use crate::check::JsonnetInput;

#[test]
fn function_arg() {
  JsonnetInput::pre_eval_error(
    r"
local uh = function(x) 3;
##                  ^ diagnostic: unused: `x`
uh(4)
",
  )
  .check();
}

#[test]
fn local() {
  JsonnetInput::pre_eval_error(
    r"
local y = 3;
##    ^ diagnostic: unused: `y`
{}
",
  )
  .check();
}
