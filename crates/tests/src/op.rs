//! Tests for operators, like `+` and `!`.

use crate::check::{Input, JsonnetInput};

#[test]
fn add_str() {
  Input::default()
    .with_jsonnet(
      "a.jsonnet",
      JsonnetInput::manifest(
        r"
'123'
",
        r#"
"123"
"#,
      ),
    )
    .with_jsonnet("b.jsonnet", JsonnetInput::manifest("4", "4"))
    .with_jsonnet(
      "c.jsonnet",
      JsonnetInput::string(
        r"
(import 'a.jsonnet') + (import 'b.jsonnet')
",
        "1234",
      ),
    )
    .add_all()
    .check();
}

#[test]
fn add_union_ok() {
  JsonnetInput::manifest(
    r#"
local f(x) =
  assert std.isNumber(x) || std.isString(x);
  x + 3;

[f(2), f("hi")]
"#,
    r#"
[5, "hi3"]
"#,
  )
  .check();
}

#[test]
fn add_union_err() {
  JsonnetInput::manifest(
    r#"
local f(x) =
  assert std.isNumber(x) || std.isObject(x);
  x + 3;
##^^^^^ diagnostic: invalid use of `+`; expected addable types, e.g. `number`, `string`, `object`, `array[any]`; left: `object`; right: `number`

f(4)
"#,
    "7",
  )
  .check();
}

#[test]
fn eq_fn() {
  JsonnetInput::eval_error(
    r#"
local f(x) = assert std.isNumber(x); x + 1;
local g(x) = assert std.isNumber(x); x + 2;
  f == g
##^^^^^^ diagnostic: invalid use of `==`; expected equatable types, i.e. anything exception `function`; left: `(x: number) => number`; right: `(x: number) => number`
"#,
    "cannot test equality of functions",
  )
  .check();
}
