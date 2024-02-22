use crate::check::{Input, JsonnetInput};

#[test]
fn add_str() {
  Input::default()
    .with_jsonnet(
      "a.jsonnet",
      JsonnetInput::manifest(
        r"
'123'
",
        r#"
"123"
"#,
      ),
    )
    .with_jsonnet("b.jsonnet", JsonnetInput::manifest("4", "4"))
    .with_jsonnet(
      "c.jsonnet",
      JsonnetInput::string(
        r"
(import 'a.jsonnet') + (import 'b.jsonnet')
",
        "1234",
      ),
    )
    .add_all()
    .check();
}
