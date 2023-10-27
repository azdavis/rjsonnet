use crate::check::{exec, manifest};

#[test]
fn int() {
  manifest("3");
}

#[test]
fn float() {
  manifest("3.4");
}

#[test]
fn str_double() {
  manifest(
    r#"
"hi"
"#,
  );
}

#[test]
fn bool_true() {
  manifest("true");
}

#[test]
fn bool_false() {
  manifest("false");
}

#[test]
fn null() {
  manifest("null");
}

#[test]
fn array_empty() {
  manifest("[]");
}

#[test]
fn array_non_empty() {
  manifest(
    r#"
[1, true, "foo"]
"#,
  );
}

#[test]
fn object_empty() {
  manifest("{}");
}

#[test]
fn object_non_empty() {
  manifest(
    r#"
{
  num: 1,
  bool: true,
  str: "bar",
  "foo quz": null,
}
"#,
  );
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
  exec("function(x) x + 1");
}

#[test]
#[should_panic = "parse error:"]
fn parse_fail() {
  manifest("if else");
}

#[test]
fn if_else() {
  manifest("if 1 < 2 then 3 else 4");
}

#[test]
fn if_no_else() {
  manifest("if 1 < 2 then 3");
}

#[test]
#[should_panic = "exec error: User"]
fn error() {
  manifest(
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
  );
}
