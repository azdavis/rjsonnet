//! Smoke tests, intended to be the smallest possible test for a given feature.

mod array;
mod comment;
mod def;
mod function;
mod keyword_lit;
mod number;
mod object;
mod op;
mod std_lib;
mod string;

use crate::check::{Input, JsonnetInput};

#[test]
fn parse_fail() {
  JsonnetInput::pre_eval_error(
    r"
if else 4
## ^^^^ diagnostic: expected expression
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
#[should_panic]
fn error() {
  JsonnetInput::eval_error(
    r#"
1 + (error "oh no!")
##   ^^^^^^^^^^^^^^ diagnostic: <eval>
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
