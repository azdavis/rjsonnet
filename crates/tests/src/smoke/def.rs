//! Tests for go-to-definition.

use crate::check::JsonnetInput;

#[test]
fn simple() {
  JsonnetInput::manifest(
    r"
local a = 3;
##        ^ def: a
1 + 2 + a
##      ^ use: a
",
    "6",
  )
  .check_one();
}

#[test]
fn chain() {
  JsonnetInput::manifest(
    r"
local a = 100;
##        ^^^ def: a
local b = a;
local c = b;
local hm = 321;
local obj = { foo: c, bar: 4 };
local uh(a) = 654 + a;
local y = obj.foo;
local z = y;

1 + 2 + z
##      ^ use: a
",
    "103",
  )
  .check_one();
}

#[test]
fn subscript() {
  JsonnetInput::manifest(
    r"
local a = {
  foo: 1,
  bar: 2,
##     ^ def: a_bar
};

a.bar
## ^ use: a_bar
",
    "2",
  )
  .check_one();
}
