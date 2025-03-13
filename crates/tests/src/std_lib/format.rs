//! Tests for string formatting.

use crate::check::JsonnetInput;

#[test]
fn smoke_parse_err() {
  JsonnetInput::manifest_or_fn(
    r#"
function()
  "hi there %(oops" % 1
##^^^^^^^^^^^^^^^^^ err: invalid format string: unexpected end of string
"#,
  )
  .check();
}

#[test]
fn smoke_bad_conversion() {
  JsonnetInput::manifest_or_fn(
    r#"
function()
  "hi there %q" % 1
##^^^^^^^^^^^^^ err: invalid format string: unrecognized conversion type: 'q'
"#,
  )
  .check();
}

#[test]
fn too_few_args() {
  JsonnetInput::manifest_or_fn(
    r#"
function()
  "hi there %d %d" % 1
##^^^^^^^^^^^^^^^^ err: wrong number of format arguments; expected 2; found 1
"#,
  )
  .check();
}

#[test]
fn too_many_args() {
  JsonnetInput::manifest_or_fn(
    r#"
function()
  "hi there %d" % [1, 2]
##^^^^^^^^^^^^^ err: wrong number of format arguments; expected 1; found 2
"#,
  )
  .check();
}

#[test]
fn wrong_ty() {
  JsonnetInput::manifest_or_fn(
    r#"
function()
  "hi there %d" % null
##^^^^^^^^^^^^^ err: incompatible types; expected `number`; found `null`
"#,
  )
  .check();
}
