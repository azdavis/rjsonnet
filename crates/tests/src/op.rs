//! Tests for operators, like `+` and `!`.

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

#[test]
fn add_union_ok() {
  JsonnetInput::manifest(
    r#"
local f(x) =
  assert std.isNumber(x) || std.isString(x);
  x + 3;

[f(2), f("hi")]
"#,
    r#"
[5, "hi3"]
"#,
  )
  .check();
}

#[test]
fn add_union_err() {
  JsonnetInput::manifest(
    r#"
local f(x) =
  assert std.isNumber(x) || std.isObject(x);
  x + 3;
##^^^^^ diagnostic: not a pair of types that can be added with `+`; left: `object`; right: `number`

f(4)
"#,
    "7",
  )
  .check();
}
