use crate::check::{manifest, num};

#[test]
fn slash_slash() {
  manifest(
    r#"
1 + // 2 +
3
"#,
    num(4.0),
  );
}

#[test]
fn hash() {
  manifest(
    r#"
1 # + 2
"#,
    num(1.0),
  );
}

#[test]
fn slash_star() {
  manifest(
    r#"
2 /* * 2 */ + 3
"#,
    num(5.0),
  );
}
