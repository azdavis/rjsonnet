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
