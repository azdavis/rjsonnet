//! Tests for test blocks, delimited with |||.

use crate::check::JsonnetInput;

#[test]
fn smoke_one() {
  JsonnetInput::string(
    r"
|||
  hello world
|||
",
    "hello world\n",
  )
  .check();
}

#[test]
fn smoke() {
  JsonnetInput::string(
    r"
|||
  hi there
  buddy
|||
",
    "hi there\nbuddy\n",
  )
  .check();
}

#[test]
#[should_panic = "must have a `|||` to end"]
fn inner_blank() {
  JsonnetInput::string(
    r"
|||
  hi there

  buddy
|||
",
    "hi there\n\nbuddy\n",
  )
  .check();
}

#[test]
fn indented() {
  JsonnetInput::string(
    r"
local t =
  |||
    hi there
    buddy
  |||;

t
",
    "hi there\nbuddy\n",
  )
  .check();
}

#[test]
fn end_delimiter_further_left() {
  JsonnetInput::string(
    r"
local t =
  |||
    hi there
    buddy
|||;

t
",
    "hi there\nbuddy\n",
  )
  .check();
}

#[test]
fn fake_end_delimiter_further_right() {
  JsonnetInput::string(
    r"
local t =
  |||
    the guy below
    is NOT the end
    |||
  |||;

t
",
    "the guy below\nis NOT the end\n|||\n",
  )
  .check();
}

#[test]
fn no_escape() {
  JsonnetInput::string(
    r"
local t =
  |||
    1\t2
    3\n4
  |||;

t
",
    "1\\t2\n3\\n4\n",
  )
  .check();
}

#[test]
fn chomp() {
  JsonnetInput::string(
    r"
|||-
  foo bar
  4
|||
",
    "foo bar\n4",
  )
  .check();
}

#[test]
fn chomp_indented() {
  JsonnetInput::string(
    r"
local t =
  |||-
    hi there
    buddy
  |||;

t
",
    "hi there\nbuddy",
  )
  .check();
}

#[test]
#[should_panic = "must have a `|||` to end"]
fn trailing_newline() {
  JsonnetInput::string(
    r"
local t =
  |||
    hi there
    buddy

  |||;

t
",
    "hi there\nbuddy\n\n",
  )
  .check();
}

#[test]
#[should_panic = "must have a `|||` to end"]
fn chomp_trailing_newline() {
  JsonnetInput::string(
    r"
local t =
  |||-
    hi there
    buddy

  |||;

t
",
    "hi there\nbuddy\n",
  )
  .check();
}
