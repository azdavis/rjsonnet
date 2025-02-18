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
##^^^^^ err: invalid use of `+`; expected addable types; left: `object`; right: `number`

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
##^^^^^^ err: invalid use of `==`; expected equatable types; left: `(x: number) => number`; right: `(x: number) => number`
"#,
    "cannot test equality of functions",
  )
  .check();
}

#[test]
fn cmp() {
  JsonnetInput::eval_error(
    r#"
[
  1 < 2,
  "a" > "b",
  [1, 2] <= [2, 3],
  ["hi"] >= ["bye"],
  1 < "no",
##^^^^^^^^ err: invalid comparison; expected comparable types; left: `number`; right: `string`
  false > 3,
##^^^^^^^^^ err: invalid comparison; expected comparable types; left: `false`; right: `number`
  null <= 4,
##^^^^^^^^^ err: invalid comparison; expected comparable types; left: `null`; right: `number`
]
"#,
    "incompatible types",
  )
  .check();
}
