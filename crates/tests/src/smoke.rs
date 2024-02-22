mod array;
mod comment;
mod keyword_lit;
mod number;
mod object;
mod op;
mod std_lib;
mod string;

use crate::check::{Input, JsonnetInput};

#[test]
fn function() {
  JsonnetInput::manifest(
    r"
local inc = function(x) x + 1;
inc(3)
",
    "4",
  )
  .check_one();
}

#[test]
fn undef_fn_arg() {
  JsonnetInput::error(
    r"
local f = function(b, x) if b then x else 1;
f(false)
",
    "parameter `x` was not defined at the function call site",
  )
  .check_one();
}

#[test]
#[should_panic = "expected expression"]
fn parse_fail() {
  JsonnetInput::manifest("if else", "0").check_one();
}

#[test]
fn if_else() {
  JsonnetInput::manifest("if 1 < 2 then 3 else 4", "3.0").check_one();
}

#[test]
fn if_without_else_yes() {
  JsonnetInput::manifest("if 1 < 2 then 3", "3.0").check_one();
}

#[test]
fn if_without_else_no() {
  JsonnetInput::manifest("if 1 > 2 then 3", "null").check_one();
}

#[test]
fn error() {
  JsonnetInput::error(
    r#"
error "oh no!"
"#,
    "explicit `error`: oh no!",
  )
  .check_one();
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
  .check_one();
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
  .check_one();
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
  .check_one();
}

#[test]
fn import() {
  Input::default()
    .with_jsonnet("a.jsonnet", JsonnetInput::manifest("1 + 2", "3"))
    .with_jsonnet("b.jsonnet", JsonnetInput::manifest("(import 'a.jsonnet') + 4", "7"))
    .add_all()
    .check();
}
