//! Smoke tests, intended to be the smallest possible test for a given feature.

use crate::check::{Input, JsonnetInput};

#[test]
fn parse_fail() {
  JsonnetInput::pre_eval_error(
    r"
## vvvv err: expected `then`, found `else`
if else 4
## ^^^^ err: expected expression, found `else`
",
  )
  .check();
}

#[test]
fn if_else() {
  JsonnetInput::manifest("if 1 < 2 then 3 else 4", "3.0").check();
}

#[test]
fn if_without_else_yes() {
  JsonnetInput::manifest("if 1 < 2 then 3", "3.0").check();
}

#[test]
fn if_without_else_no() {
  JsonnetInput::manifest("if 1 > 2 then 3", "null").check();
}

#[test]
fn error() {
  JsonnetInput::eval_error(
    r#"
1 + (error "oh no!")
"#,
    "explicit `error`: oh no!",
  )
  .check();
}

#[test]
fn assert() {
  JsonnetInput::manifest(
    r#"
assert 2 + 2 < 5 : "math makes sense";
0
"#,
    "0.0",
  )
  .check();
}

#[test]
fn local() {
  JsonnetInput::manifest(
    r"
local x = 3;
x + 1
",
    "4.0",
  )
  .check();
}

#[test]
fn bool_op() {
  JsonnetInput::manifest(
    r"
[false || true, true && false, !true]
",
    r"
[true, false, false]
",
  )
  .check();
}

#[test]
fn import() {
  Input::default()
    .with_jsonnet("a.jsonnet", JsonnetInput::manifest("1 + 2", "3"))
    .with_jsonnet("b.jsonnet", JsonnetInput::manifest("(import 'a.jsonnet') + 4", "7"))
    .add_all()
    .check();
}

#[test]
fn join_str() {
  JsonnetInput::manifest(
    r#"
std.join("!", ["a", "b", "c"])
"#,
    r#"
"a!b!c"
"#,
  )
  .check();
}

#[test]
fn join_arr() {
  JsonnetInput::manifest(
    r#"
std.join([1], [[2], [4, 5], [6]])
"#,
    r#"
[2, 1, 4, 5, 1, 6]
"#,
  )
  .check();
}

#[test]
fn pass_arg() {
  JsonnetInput::manifest(
    r"
local a(x) = x;
local b(y) = a(y);

b(3)
",
    "3",
  )
  .check();
}

#[test]
fn shadow_arg() {
  JsonnetInput::manifest(
    r"
local x = 2;
local a(x) = x;

x + a(3) + x
",
    "7",
  )
  .check();
}
