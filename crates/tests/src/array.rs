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

#[test]
fn set() {
  JsonnetInput::manifest_or_fn(
    r"
local xs = std.set([1, 2]);
local ys = [2, 3];
function()
  std.setInter(xs, ys)
##                 ^^ err: incompatible types; expected `set[any]`; found `tuple[number, number]`
",
  )
  .check();
}

#[test]
#[should_panic = "no diagnostics"]
fn comp_not_array() {
  JsonnetInput::manifest_or_fn(
    r#"
function(obj)
  assert std.isObject(obj);
  [x for x in obj]
##            ^^^ err: expected `array[any]`; found `object`
  "#,
  )
  .check();
}
