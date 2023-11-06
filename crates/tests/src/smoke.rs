mod array;
mod comment;
mod keyword_lit;
mod number;
mod object;
mod string;

use crate::check::{exec, manifest};

#[test]
fn function() {
  let (_, got) = exec("function(x) x + 1");
  assert!(matches!(
    got,
    jsonnet_eval::val::Val::Rec { env: _, kind: jsonnet_eval::val::RecValKind::Function { .. } }
  ));
}

#[test]
#[should_panic = "parse error:"]
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
#[should_panic = "kind: User"]
fn error() {
  exec(
    r#"
error "oh no!"
"#,
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
    r#"
local x = 3;
x + 1
"#,
    "4.0",
  );
}
