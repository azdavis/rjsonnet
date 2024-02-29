//! Tests for go-to-definition.

use crate::check::{Input, JsonnetInput};

#[test]
fn simple() {
  JsonnetInput::manifest(
    r"
local a = 3;
##        ^ def: a
1 + 2 + a
##      ^ use: a
",
    "6",
  )
  .check_one();
}

#[test]
fn chain() {
  JsonnetInput::manifest(
    r"
local a = 100;
##        ^^^ def: a
local b = a;
local c = b;
local hm = 321;
local obj = { foo: c, bar: 4 };
local uh(a) = 654 + a;
local y = obj.foo;
local z = y;

1 + 2 + z
##      ^ use: a
",
    "103",
  )
  .check_one();
}

#[test]
fn subscript() {
  JsonnetInput::manifest(
    r"
local a = {
  foo: 1,
  bar: 2,
##     ^ def: a_bar
};

a.bar
## ^ use: a_bar
",
    "2",
  )
  .check_one();
}

#[test]
fn import_minimal() {
  Input::default()
    .with_jsonnet(
      "a.jsonnet",
      JsonnetInput::manifest(
        r"
  3
##^ def: file_a
",
        "3",
      ),
    )
    .with_jsonnet(
      "b.jsonnet",
      JsonnetInput::manifest(
        r"
import 'a.jsonnet'
## ^ use: file_a
",
        "3",
      ),
    )
    .add("b.jsonnet")
    .check();
}

#[test]
fn import() {
  Input::default()
    .with_jsonnet(
      "a.jsonnet",
      JsonnetInput::manifest(
        r"
  1 + 2
##^^^^^ def: file_a
",
        "3",
      ),
    )
    .with_jsonnet(
      "b.jsonnet",
      JsonnetInput::manifest(
        r"
(import 'a.jsonnet') + 4
## ^ use: file_a
",
        "7",
      ),
    )
    .add("b.jsonnet")
    .check();
}

#[test]
fn import_local() {
  Input::default()
    .with_jsonnet(
      "a.jsonnet",
      JsonnetInput::manifest(
        r"
{
  num: 1 + 4,
##     ^^^^^ def: a_num
}
",
        r#"
{
  "num": 5
}
"#,
      ),
    )
    .with_jsonnet(
      "b.jsonnet",
      JsonnetInput::manifest(
        r"
local a = import 'a.jsonnet';
a.num - 3
## ^ use: a_num
",
        "2",
      ),
    )
    .add("b.jsonnet")
    .check();
}

#[test]
fn small_chain_1() {
  Input::default()
    .with_jsonnet(
      "a.jsonnet",
      JsonnetInput::manifest(
        r#"
  "hi"
"#,
        r#"
"hi"
"#,
      ),
    )
    .with_jsonnet(
      "b.jsonnet",
      JsonnetInput::manifest(
        r#"
local two = import 'a.jsonnet';
  two + "!"
##^^^^^^^^^ def: the_file
"#,
        r#"
"hi!"
"#,
      ),
    )
    .with_jsonnet(
      "d.jsonnet",
      JsonnetInput::manifest(
        r#"
local three = import 'b.jsonnet';
  [three]
## ^ use: the_file
"#,
        r#"
[
  "hi!"
]
"#,
      ),
    )
    .add_all()
    .check();
}

#[test]
fn small_chain_2() {
  Input::default()
    .with_jsonnet(
      "a.jsonnet",
      JsonnetInput::manifest(
        r#"
  "hi"
##^^^^ def: the_file
// TODO fix this def. it should be on the second file instead
"#,
        r#"
"hi"
"#,
      ),
    )
    .with_jsonnet(
      "c.jsonnet",
      JsonnetInput::manifest(
        r#"
local two = import 'a.jsonnet';
  two + "!"
"#,
        r#"
"hi!"
"#,
      ),
    )
    .with_jsonnet(
      "d.jsonnet",
      JsonnetInput::manifest(
        r#"
local three = import 'c.jsonnet';
  [three]
## ^ use: the_file
"#,
        r#"
[
  "hi!"
]
"#,
      ),
    )
    .add_all()
    .check();
}
