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

#[test]
fn prune() {
  JsonnetInput::eval_error(
    r#"
local xs = [2, 4, null, 6, {}];
##    ^^ type: array[null | number | {}]
local ys = std.prune(xs);
##    ^^ type: array[number]
local sc1 = std.prune({});
##    ^^^ type: {}
local sc2 = std.prune(null);
##    ^^^ type: null

[std.length(ys), sc1, sc2]
"#,
    "not yet implemented: prune",
  )
  .check();
}
