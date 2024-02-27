//! Miscellaneous tests.
//!
//! If you're not sure where a test should go, put it here.

use crate::check::{Input, JsonnetInput};

#[test]
fn func_arg_id_not_named_arg() {
  JsonnetInput::manifest(
    r"
local obj = { field: 3 };
local func(x, y) = x;
func(obj.field, 4)
",
    "3",
  )
  .check_one();
}

#[test]
#[should_panic = "not yet implemented: std.makeArray"]
fn for_comp_obj() {
  JsonnetInput::manifest(
    r#"
{
  [x]: false
  for x in ["a", "b"]
}
"#,
    r#"
{
  "a": false,
  "b": false
}
"#,
  )
  .check_one();
}

#[test]
fn import_chain() {
  Input::default()
    .with_jsonnet("a.jsonnet", JsonnetInput::manifest("6 - 5 + 2", "3"))
    .with_jsonnet("b.jsonnet", JsonnetInput::manifest("(import 'a.jsonnet') + 2", "5"))
    .with_jsonnet("c.jsonnet", JsonnetInput::manifest("(import 'b.jsonnet') + 4", "9"))
    .add("c.jsonnet")
    .check();
}

#[test]
fn import_self() {
  Input::default()
    .with_jsonnet(
      "a.jsonnet",
      JsonnetInput::manifest(
        r#"
{
  foo: (import "a.jsonnet").bar + 1,
  bar: 3,
}
"#,
        r#"
{
  "foo": 4,
  "bar": 3
}
"#,
      ),
    )
    .add_all()
    .check();
}

#[test]
fn import_str() {
  Input::default()
    .with_jsonnet(
      "a.jsonnet",
      JsonnetInput::manifest(
        r#"
importstr "hi.txt"
"#,
        r#"
"hello there"
"#,
      ),
    )
    .with_raw("hi.txt", "hello there")
    .add("a.jsonnet")
    .check();
}
