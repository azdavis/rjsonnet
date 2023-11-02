use crate::check::{exec, manifest};
use jsonnet_eval::manifest::Val;
use jsonnet_expr::{Number, Prim};
use std::collections::HashMap;

#[test]
fn int() {
  let want = Val::Prim(Prim::Number(Number::try_from(3.0).unwrap()));
  let got = manifest("3");
  assert_eq!(want, got);
}

#[test]
fn float() {
  let want = Val::Prim(Prim::Number(Number::try_from(3.4).unwrap()));
  let got = manifest("3.4");
  assert_eq!(want, got);
}

/// TODO figure out how to assert eq for this test?
#[test]
fn str_double() {
  let got = manifest(
    r#"
"hi"
"#,
  );
  assert!(matches!(got, Val::Prim(Prim::String(_))));
}

#[test]
fn str_single() {
  let got = manifest(
    r#"
'hi'
"#,
  );
  assert!(matches!(got, Val::Prim(Prim::String(_))));
}

#[test]
fn str_double_verbatim() {
  let got = manifest(
    r#"
@"hi"
"#,
  );
  assert!(matches!(got, Val::Prim(Prim::String(_))));
}

#[test]
fn str_single_verbatim() {
  let got = manifest(
    r#"
@'hi'
"#,
  );
  assert!(matches!(got, Val::Prim(Prim::String(_))));
}

#[test]
fn bool_true() {
  let want = Val::Prim(Prim::Bool(true));
  let got = manifest("true");
  assert_eq!(want, got);
}

#[test]
fn bool_false() {
  let want = Val::Prim(Prim::Bool(false));
  let got = manifest("false");
  assert_eq!(want, got);
}

#[test]
fn null() {
  let want = Val::Prim(Prim::Null);
  let got = manifest("null");
  assert_eq!(want, got);
}

#[test]
fn array_empty() {
  let want = Val::Array(Vec::new());
  let got = manifest("[]");
  assert_eq!(want, got);
}

#[test]
fn array_non_empty() {
  let got = manifest(
    r#"
[1, true, "foo"]
"#,
  );
  let Val::Array(vs) = &got else { panic!("not an array") };
  let [fst, snd, thd] = &vs[..] else { panic!("not length 3") };
  assert_eq!(Val::Prim(Prim::Number(Number::positive_one())), *fst);
  assert_eq!(Val::Prim(Prim::Bool(true)), *snd);
  assert!(matches!(*thd, Val::Prim(Prim::String(_))));
}

#[test]
fn object_empty() {
  let want = Val::Object(HashMap::default());
  let got = manifest("{}");
  assert_eq!(want, got);
}

#[test]
fn object_non_empty() {
  let got = manifest(
    r#"
{
  num: 1,
  bool: true,
  str: "bar",
  "foo quz": null,
}
"#,
  );
  assert!(matches!(got, Val::Object(_)));
}

#[test]
#[should_panic = "+ for non-prim"]
fn object_implicit_plus() {
  manifest(
    r#"
{ a: 1 } { b: 2 }
"#,
  );
}

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
