//! Tests for import/importstr/importbin.

use crate::check::{Input, JsonnetInput, MultiInput};

#[test]
fn chain() {
  Input::default()
    .with_jsonnet("a.jsonnet", JsonnetInput::manifest("6 - 5 + 2", "3"))
    .with_jsonnet("b.jsonnet", JsonnetInput::manifest("(import 'a.jsonnet') + 2", "5"))
    .with_jsonnet("c.jsonnet", JsonnetInput::manifest("(import 'b.jsonnet') + 4", "9"))
    .add("c.jsonnet")
    .check();
}

#[test]
fn of_self() {
  Input::default()
    .with_jsonnet(
      "a.jsonnet",
      JsonnetInput::manifest(
        r#"
{
  foo: (import "a.jsonnet").bar + 1,
  bar: 3,
}
"#,
        r#"
{
  "foo": 4,
  "bar": 3
}
"#,
      ),
    )
    .add_all()
    .check();
}

#[test]
fn str() {
  Input::default()
    .with_jsonnet(
      "a.jsonnet",
      JsonnetInput::manifest(
        r#"
importstr "hi.txt"
"#,
        r#"
"hello there"
"#,
      ),
    )
    .with_raw("hi.txt", "hello there")
    .add("a.jsonnet")
    .check();
}

#[test]
fn bin() {
  Input::default()
    .with_jsonnet(
      "a.jsonnet",
      JsonnetInput::manifest(
        r#"
importbin "hi.txt"
"#,
        r#"
[104, 101, 121]
"#,
      ),
    )
    .with_raw("hi.txt", "hey")
    .add("a.jsonnet")
    .check();
}

#[test]
fn cycle() {
  Input::default()
    .with_jsonnet(
      "a.jsonnet",
      JsonnetInput::eval_error(
        r"
  import 'a.jsonnet'
",
        "import cycle: a.jsonnet -> a.jsonnet",
      ),
    )
    .add_all()
    .check();
}

#[test]
fn bigger_cycle() {
  Input::default()
    .with_jsonnet(
      "a.jsonnet",
      JsonnetInput::eval_error(
        r"
  import 'b.jsonnet'
",
        "import cycle: b.jsonnet -> c.jsonnet -> d.jsonnet -> a.jsonnet -> b.jsonnet",
      ),
    )
    .with_jsonnet(
      "b.jsonnet",
      JsonnetInput::eval_error(
        r"
  import 'c.jsonnet'
",
        "import cycle: c.jsonnet -> d.jsonnet -> a.jsonnet -> b.jsonnet -> c.jsonnet",
      ),
    )
    .with_jsonnet(
      "c.jsonnet",
      JsonnetInput::eval_error(
        r"
  import 'd.jsonnet'
",
        "import cycle: d.jsonnet -> a.jsonnet -> b.jsonnet -> c.jsonnet -> d.jsonnet",
      ),
    )
    .with_jsonnet(
      "d.jsonnet",
      JsonnetInput::eval_error(
        r"
  import 'a.jsonnet'
",
        "import cycle: a.jsonnet -> b.jsonnet -> c.jsonnet -> d.jsonnet -> a.jsonnet",
      ),
    )
    .add_all()
    .check();
}

#[test]
fn remove() {
  MultiInput::default()
    .with_input(
      Input::default()
        .with_jsonnet("a.jsonnet", JsonnetInput::manifest(r"(import 'b.jsonnet') + 3", "5"))
        .with_jsonnet("b.jsonnet", JsonnetInput::manifest(r"1 + 1", "2"))
        .add("a.jsonnet"),
    )
    .with_input(
      Input::default()
        .with_jsonnet(
          "a.jsonnet",
          JsonnetInput::pre_eval_error(
            r"
  (import 'b.jsonnet') + 2
## ^^^^^^^^^^^^^^^^^^ err: path not found: `b.jsonnet`
",
          ),
        )
        .add("a.jsonnet")
        .remove("b.jsonnet"),
    )
    .check();
}

#[test]
fn update() {
  MultiInput::default()
    .with_input(
      Input::default()
        .with_jsonnet("a.jsonnet", JsonnetInput::manifest("(import 'b.jsonnet') + 3", "5"))
        .with_jsonnet("b.jsonnet", JsonnetInput::manifest("1 + 1", "2"))
        .add("a.jsonnet"),
    )
    .with_input(
      Input::default()
        .with_jsonnet("a.jsonnet", JsonnetInput::manifest("(import 'b.jsonnet') + 3", "7"))
        .with_jsonnet("b.jsonnet", JsonnetInput::manifest("2 + 2", "4"))
        .add("b.jsonnet"),
    )
    .check();
}

#[test]
fn text_block() {
  JsonnetInput::pre_eval_error_one(
    r#"
import |||
  bad.txt
|||"#,
    "cannot import a text block",
  )
  .check();
}
