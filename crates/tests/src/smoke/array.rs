use crate::check::{manifest, manifest_self};

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

#[test]
fn plus() {
  manifest(
    r#"
[1, 3] + [2, 4]
"#,
    r#"
[1, 3, 2, 4]
"#,
  );
}
