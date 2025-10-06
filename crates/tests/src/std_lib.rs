//! Tests for the standard library.

use crate::check::JsonnetInput;

mod format;

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
fn get() {
  JsonnetInput::manifest(
    r#"
std.get({ a: 1 }, "a", error "no")
"#,
    "1",
  )
  .check();
}

#[test]
fn prune() {
  JsonnetInput::manifest_or_fn(
    r#"
function(xs)
##       ^^ type: array[null | number | {}]
  assert std.isArray(xs) && std.all(std.map(function(x) x == null || std.isNumber(x) || (std.isObject(x) && std.length(x) == 0), xs));
  local ys = std.prune(xs);
##      ^^ type: array[number]
  local sc1 = std.prune({});
##      ^^^ type: {}
  local sc2 = std.prune(null);
##      ^^^ type: null

  [std.length(ys), sc1, sc2]
"#,
  )
  .check();
}

#[test]
fn prune_tup() {
  JsonnetInput::manifest_or_fn(
    r#"
function()
  local xs = [1, 2, null, false, {}, 5, []];
  local ys = std.prune(xs);
##      ^^ type: tuple[number, number, false, number]
  ys
"#,
  )
  .check();
}

#[test]
fn make_array_calls_lazy() {
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
fn make_array_len_zero_func_eager() {
  JsonnetInput::eval_error(
    r#"
std.makeArray(0, error "func")
"#,
    "func",
  )
  .check();
}

#[test]
fn length_num() {
  JsonnetInput::eval_error(
    r#"
std.length(3)
##         ^ err: invalid call to `std.length`; expected a type with length; found `number`
"#,
    "incompatible types",
  )
  .check();
}

#[test]
#[should_panic = "not yet implemented: flatMap"]
fn flat_map() {
  JsonnetInput::manifest(
    r#"
local xs = std.flatMap(function(c) c + c, "a1");
##    ^^ type: string
local ys = std.flatMap(function(x) if x == 3 then [] else [x, x - 1], [1, 3, 7, 5]);
##    ^^ type: array[number]

{ xs: xs, ys: ys }
"#,
    r#"
{
  "xs": "aa11",
  "ys": [1, 0, 7, 6, 5, 4],
}
"#,
  )
  .check();
}

// TODO fix
#[test]
#[should_panic = "unreachable code"]
fn object_values_comp() {
  JsonnetInput::manifest_or_fn(
    r#"
function(foo)
  [x.bar for x in std.objectValues(foo)]
"#,
  )
  .check();
}

#[test]
fn map_with_key_override_vis() {
  JsonnetInput::manifest(
    r#"
local f(a, b) = std.length(a) + b;
local vis = {foo::: 3};
local hid = {foo:: 2};
{
  x: hid + vis,
  y: hid + std.mapWithKey(f, vis),
}
"#,
    r#"
{
  "x": {
    "foo": 3
  },
  "y": {}
}
"#,
  )
  .check();
}

#[test]
fn join() {
  JsonnetInput::manifest(
    r#"
local s(x) = assert std.isString(x); x;
local f(x) = s(std.join(",", x));
f(['a', 'b'])
"#,
    r#"
"a,b"
"#,
  )
  .check();
}
