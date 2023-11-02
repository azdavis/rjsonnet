mod array;
mod keyword_lit;
mod number;
mod object;
mod string;

use crate::check::{exec, manifest};
use jsonnet_eval::manifest::Val;
use jsonnet_expr::{Number, Prim};

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
  manifest("if else");
}

#[test]
fn if_else() {
  let want = Val::Prim(Prim::Number(Number::try_from(3.0).unwrap()));
  let got = manifest("if 1 < 2 then 3 else 4");
  assert_eq!(want, got);
}

#[test]
fn if_without_else_yes() {
  let want = Val::Prim(Prim::Number(Number::try_from(3.0).unwrap()));
  let got = manifest("if 1 < 2 then 3");
  assert_eq!(want, got);
}

#[test]
fn if_without_else_no() {
  let want = Val::Prim(Prim::Null);
  let got = manifest("if 1 > 2 then 3");
  assert_eq!(want, got);
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
  let want = Val::Prim(Prim::Number(Number::positive_zero()));
  let got = manifest(
    r#"
assert 2 + 2 < 5 : "math makes sense";
0
"#,
  );
  assert_eq!(want, got);
}

#[test]
fn local() {
  let want = Val::Prim(Prim::Number(Number::try_from(4.0).unwrap()));
  let got = manifest(
    r#"
local x = 3;
x + 1
"#,
  );
  assert_eq!(want, got);
}
