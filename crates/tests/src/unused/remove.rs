//! Removing unused locals.

use crate::check::JsonnetInput;

#[test]
fn smoke() {
  JsonnetInput::rm_unused(
    r#"
local x = 1;
2
"#,
    r#"2
"#,
  )
  .check();
}

#[test]
fn multi_binding_not_all() {
  JsonnetInput::rm_unused(
    r#"
local x = 1, y = 2;
y
"#,
    r#"
local y = 2;
y
"#,
  )
  .check();
}

#[test]
fn multi_binding_all() {
  JsonnetInput::rm_unused(
    r#"
local x = 1, y = 2;
3
"#,
    r#"3
"#,
  )
  .check();
}

#[test]
#[should_panic = "should remove unused"]
fn obj() {
  JsonnetInput::rm_unused(
    r#"
{
  local x = 1,
  a: 2,
}
"#,
    r#"
{
  a: 2,
}
"#,
  )
  .check();
}
