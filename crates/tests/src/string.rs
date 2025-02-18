//! Tests for strings, e.g. escape sequences like \n.

use crate::check::JsonnetInput;

#[test]
fn double() {
  JsonnetInput::string(
    r#"
"hi"
"#,
    "hi",
  )
  .check();
}

#[test]
fn single() {
  JsonnetInput::string("'hi'", "hi").check();
}

#[test]
fn double_escape() {
  JsonnetInput::string(
    r#"
"hi\nthere\"my'friend\'buddy"
"#,
    "hi\nthere\"my'friend'buddy",
  )
  .check();
}

#[test]
fn single_escape() {
  JsonnetInput::string(
    r#"
'hi\nthere"my\'friend\"buddy'
"#,
    "hi\nthere\"my'friend\"buddy",
  )
  .check();
}

#[test]
fn double_verbatim() {
  JsonnetInput::string(
    r#"
@"hi"
"#,
    "hi",
  )
  .check();
}

#[test]
fn single_verbatim() {
  JsonnetInput::string(
    r"
@'hi'
", "hi",
  )
  .check();
}

#[test]
fn double_verbatim_escape() {
  JsonnetInput::string(
    r#"
@"hi "" '' \\ \n there"
"#,
    r#"hi " '' \\ \n there"#,
  )
  .check();
}

#[test]
fn unclosed() {
  JsonnetInput::pre_eval_error(
    r"
## V err: unclosed string
  '",
  )
  .check();
}

#[test]
fn text_block() {
  JsonnetInput::string(
    r"
|||
  hi there
  buddy
|||",
    "hi there\nbuddy\n",
  )
  .check();
}
