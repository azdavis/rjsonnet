//! Miscellaneous tests.
//!
//! If you're not sure where a test should go, put it here.

use crate::check::{Input, JsonnetInput, MultiInput};

#[test]
fn func_arg_id_not_named_arg() {
  JsonnetInput::manifest(
    r"
local obj = { field: 3 };
local func(x, y) = if y == 4 then x;
func(obj.field, 4)
",
    "3",
  )
  .check();
}

#[test]
fn for_comp_obj() {
  JsonnetInput::eval_error(
    r#"
{
  [x]: std.length(x)
  for x in ["foo", "bar quz"]
}
"#,
    "not yet implemented: std.makeArray",
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
fn import_chain() {
  Input::default()
    .with_jsonnet("a.jsonnet", JsonnetInput::manifest("6 - 5 + 2", "3"))
    .with_jsonnet("b.jsonnet", JsonnetInput::manifest("(import 'a.jsonnet') + 2", "5"))
    .with_jsonnet("c.jsonnet", JsonnetInput::manifest("(import 'b.jsonnet') + 4", "9"))
    .add("c.jsonnet")
    .check();
}

#[test]
fn import_self() {
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
fn import_str() {
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
fn self_cycle() {
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
## ^^^^^^^^^^^^^^^^^^ diagnostic: path not found: b.jsonnet
",
          ),
        )
        .add("a.jsonnet")
        .remove("b.jsonnet"),
    )
    .check();
}

#[test]
fn obj_array_self() {
  JsonnetInput::manifest(
    r#"
{
  foo: [
    { kind: 'Soda', qty: 2 },
  ],
  quz: self.foo,
}
"#,
    r#"
{
  "foo": [{ "kind": "Soda", "qty": 2 }],
  "quz": [{ "kind": "Soda", "qty": 2 }]
}
"#,
  )
  .check();
}

#[test]
fn same_fn_ty() {
  JsonnetInput::pre_eval_error(
    r#"
local foo() = null;
##    ^^^ diagnostic: unused: `foo`
local bar() = 1;
##    ^^^ diagnostic: unused: `bar`
local quz() = 2;

quz()
"#,
  )
  .check();
}

#[test]
fn length_num() {
  JsonnetInput::pre_eval_error(
    r#"
std.length(3)
##         ^ diagnostic: not a type which has length: `number`
"#,
  )
  .check();
}
