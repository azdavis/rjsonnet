use crate::check::manifest_self;

#[test]
fn empty() {
  manifest_self("[]");
}

#[test]
fn non_empty() {
  manifest_self(
    r#"
[1, true, "foo"]
"#,
  );
}
