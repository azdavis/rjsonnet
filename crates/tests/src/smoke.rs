use crate::check::{exec, manifest};

#[test]
fn int() {
  manifest(
    r#"
3
"#,
  );
}

#[test]
fn float() {
  manifest(
    r#"
3.4
"#,
  );
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
  manifest(
    r#"
true
"#,
  );
}

#[test]
fn bool_false() {
  manifest(
    r#"
false
"#,
  );
}

#[test]
fn null() {
  manifest(
    r#"
null
"#,
  );
}

#[test]
fn array() {
  manifest(
    r#"
[1, true, "foo"]
"#,
  );
}

#[test]
fn object() {
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
fn function() {
  exec(
    r#"
function(x) x + 1
"#,
  );
}

#[test]
#[should_panic = "parse error:"]
fn parse_fail() {
  manifest(
    r#"
if else
"#,
  );
}

#[test]
fn if_else() {
  manifest(
    r#"
if 1 < 2 then 3 else 4
"#,
  );
}

#[test]
fn if_no_else() {
  manifest(
    r#"
if 1 < 2 then 3
"#,
  );
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
