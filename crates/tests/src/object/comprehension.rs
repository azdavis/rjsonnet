//! Tests for objects comprehensions.

use crate::check::JsonnetInput;

#[test]
fn smoke() {
  JsonnetInput::manifest(
    r#"
{
  [x]: std.length(x)
  for x in ["foo", "bar quz"]
}
"#,
    r#"
{
  "foo": 3,
  "bar quz": 7
}
"#,
  )
  .check();
}

#[test]
fn plus() {
  JsonnetInput::manifest(
    r#"
{
  [x]+: "hi"
  for x in ["a", "b"]
}
"#,
    r#"
{
  "a": "hi",
  "b": "hi"
}
"#,
  )
  .check();
}

#[test]
fn no_params() {
  JsonnetInput::pre_eval_error(
    r#"
{
  [x](y): "hi"
##   ^^^ diagnostic: object comprehension field must not have parameters
  for x in ["a", "b"]
}
"#,
  )
  .check();
}
