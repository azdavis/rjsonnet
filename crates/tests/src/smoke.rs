mod array;
mod comment;
mod keyword_lit;
mod number;
mod object;
mod op;
mod std_lib;
mod string;

use crate::check::{exec_err, manifest, manifest_many};

#[test]
fn function() {
  manifest(
    r"
local inc = function(x) x + 1;
inc(3)
",
    "4",
  );
}

#[test]
fn undef_fn_arg() {
  exec_err(
    r"
local f = function(b, x) if b then x else 1;
f(false)
",
    "parameter `x` was not defined at the function call site",
  );
}

#[test]
#[should_panic = "expected expression"]
fn parse_fail() {
  manifest("if else", "0");
}

#[test]
fn if_else() {
  manifest("if 1 < 2 then 3 else 4", "3.0");
}

#[test]
fn if_without_else_yes() {
  manifest("if 1 < 2 then 3", "3.0");
}

#[test]
fn if_without_else_no() {
  manifest("if 1 > 2 then 3", "null");
}

#[test]
fn error() {
  exec_err(
    r#"
error "oh no!"
"#,
    "explicit `error`: oh no!",
  );
}

#[test]
fn assert() {
  manifest(
    r#"
assert 2 + 2 < 5 : "math makes sense";
0
"#,
    "0.0",
  );
}

#[test]
fn local() {
  manifest(
    r"
local x = 3;
x + 1
",
    "4.0",
  );
}

#[test]
fn bool_op() {
  manifest(
    r"
[false || true, true && false, !true]
",
    r"
[true, false, false]
",
  );
}

#[test]
fn import() {
  manifest_many(&[
    ("a.jsonnet", "1 + 2", "3"),
    (
      "b.jsonnet",
      r"
(import 'a.jsonnet') + 4
",
      "7",
    ),
  ]);
}
