use crate::check::pass;

#[test]
fn int() {
  pass(
    r#"
3
"#,
  );
}

#[test]
fn float() {
  pass(
    r#"
3.4
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

#[test]
fn object() {
  pass(
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