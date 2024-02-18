use crate::check::manifest_many;

#[test]
fn add_str() {
  manifest_many(&[
    (
      "/a.jsonnet",
      r"
'123'
",
      r#"
"123"
"#,
    ),
    (
      "/b.jsonnet",
      r"
4
",
      r"
4
",
    ),
    (
      "/c.jsonnet",
      r"
(import 'a.jsonnet') + (import 'b.jsonnet')
",
      r#"
"1234"
"#,
    ),
  ]);
}
