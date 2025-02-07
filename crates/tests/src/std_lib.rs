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

#[test]
fn this_file() {
  JsonnetInput::string(
    r"
##  v type: string
std.thisFile
",
    "/f.jsonnet",
  )
  .check();
}

#[test]
fn pi() {
  JsonnetInput::manifest(
    r"
##             v type: number
std.round(std.pi)
",
    "3",
  )
  .check();
}

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

/// TODO the calls should be lazy
#[test]
#[should_panic = "explicit `error`: zero"]
fn make_array_lazy() {
  JsonnetInput::manifest(
    r#"
local xs = std.makeArray(3, function(x) if x == 0 then error "zero" else x + 3);
xs[1]
"#,
    "4",
  )
  .check();
}

#[test]
fn length_num() {
  JsonnetInput::eval_error(
    r#"
std.length(3)
##         ^ diagnostic: invalid call to `std.length`; expected a type with length, e.g. `array[any]`, `object`, `string`, `function`; found `number`
"#,
    "incompatible types",
  )
  .check();
}
