use crate::check::manifest;

#[test]
fn slash_slash() {
  manifest(
    r"
1 + // 2 +
3
",
    "4.0",
  );
}

#[test]
fn hash() {
  manifest(
    r"
1 # + 2
",
    "1.0",
  );
}

#[test]
fn slash_star() {
  manifest(
    r"
2 /* * 2 */ + 3
",
    "5.0",
  );
}
