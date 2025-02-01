//! Tests for objects comprehensions.

use crate::check::JsonnetInput;

#[test]
fn smoke() {
  JsonnetInput::eval_error(
    r#"
{
  [x]: std.length(x)
  for x in ["foo", "bar quz"]
}
"#,
    "not yet implemented: makeArray",
  )
  .check();
  /*
  {
    "a": 3,
    "b": 7
  }
   */
}

#[test]
fn plus() {
  JsonnetInput::eval_error(
    r#"
{
  [x]+: "hi"
  for x in ["a", "b"]
}
"#,
    "not yet implemented: makeArray",
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
