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
  .check_one();
}

#[test]
fn single() {
  JsonnetInput::string("'hi'", "hi").check_one();
}

#[test]
fn double_escape() {
  JsonnetInput::string(
    r#"
"hi\nthere\"my'friend\'buddy"
"#,
    "hi\nthere\"my'friend'buddy",
  )
  .check_one();
}

#[test]
fn single_escape() {
  JsonnetInput::string(
    r#"
'hi\nthere"my\'friend\"buddy'
"#,
    "hi\nthere\"my'friend\"buddy",
  )
  .check_one();
}

#[test]
fn double_verbatim() {
  JsonnetInput::string(
    r#"
@"hi"
"#,
    "hi",
  )
  .check_one();
}

#[test]
fn single_verbatim() {
  JsonnetInput::string(
    r"
@'hi'
", "hi",
  )
  .check_one();
}

#[test]
fn double_verbatim_escape() {
  JsonnetInput::string(
    r#"
@"hi "" '' \\ \n there"
"#,
    r#"hi " '' \\ \n there"#,
  )
  .check_one();
}

#[test]
#[should_panic = "unclosed string"]
fn unclosed() {
  JsonnetInput::string("'", "").check_one();
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
  .check_one();
}
