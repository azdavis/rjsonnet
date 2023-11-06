use crate::check::manifest_str;

#[test]
fn double() {
  manifest_str(
    r#"
"hi"
"#,
    "hi",
  );
}

#[test]
fn single() {
  manifest_str("'hi'", "hi");
}

#[test]
fn double_escape() {
  manifest_str(
    r#"
"hi\nthere\"my'friend\'buddy"
"#,
    "hi\nthere\"my'friend'buddy",
  );
}

#[test]
fn single_escape() {
  manifest_str(
    r#"
'hi\nthere"my\'friend\"buddy'
"#,
    "hi\nthere\"my'friend\"buddy",
  );
}

#[test]
fn double_verbatim() {
  manifest_str(
    r#"
@"hi"
"#,
    "hi",
  );
}

#[test]
fn single_verbatim() {
  manifest_str(
    r#"
@'hi'
"#,
    "hi",
  );
}

#[test]
fn double_verbatim_escape() {
  manifest_str(
    r#"
@"hi "" '' \\ \n there"
"#,
    r#"hi " '' \\ \n there"#,
  );
}

#[test]
#[should_panic = "lex error: unclosed string"]
fn unclosed() {
  manifest_str("'", "");
}
