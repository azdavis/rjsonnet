use crate::check::JsonnetInput;

#[test]
fn empty() {
  JsonnetInput::manifest_self("[]").check_one();
}

#[test]
fn non_empty() {
  JsonnetInput::manifest_self(
    r#"
[1, true, "foo"]
"#,
  )
  .check_one();
}

#[test]
fn plus() {
  JsonnetInput::manifest(
    r"
[1, 3] + [2, 4]
",
    r"
[1, 3, 2, 4]
",
  )
  .check_one();
}
