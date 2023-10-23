use crate::check::pass;

#[test]
fn num() {
  pass(
    r#"
3
"#,
  );
}

#[test]
fn str_double() {
  pass(
    r#"
"hi"
"#,
  );
}

#[test]
fn bool_true() {
  pass(
    r#"
true
"#,
  );
}

#[test]
fn bool_false() {
  pass(
    r#"
false
"#,
  );
}

#[test]
fn null() {
  pass(
    r#"
null
"#,
  );
}

#[test]
fn array() {
  pass(
    r#"
[1, true, "foo"]
"#,
  );
}
