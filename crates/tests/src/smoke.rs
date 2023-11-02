mod array;
mod comment;
mod keyword_lit;
mod number;
mod object;
mod string;

use crate::check::{exec, manifest, manifest_raw, num};
use jsonnet_eval::manifest::Val;
use jsonnet_expr::Prim;

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
  manifest_raw("if else");
}

#[test]
fn if_else() {
  manifest("if 1 < 2 then 3 else 4", num(3.0));
}

#[test]
fn if_without_else_yes() {
  manifest("if 1 < 2 then 3", num(3.0));
}

#[test]
fn if_without_else_no() {
  manifest("if 1 > 2 then 3", Val::Prim(Prim::Null));
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
    num(0.0),
  );
}

#[test]
fn local() {
  manifest(
    r#"
local x = 3;
x + 1
"#,
    num(4.0),
  );
}
