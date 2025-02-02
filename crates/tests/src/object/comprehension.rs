//! Tests for objects comprehensions.

use crate::check::JsonnetInput;

#[test]
#[should_panic = "not yet implemented: makeArray"]
fn smoke() {
  JsonnetInput::eval_error(
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
#[should_panic = "not yet implemented: makeArray"]
fn plus() {
  JsonnetInput::eval_error(
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
