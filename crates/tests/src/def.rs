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
  .check();
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

1 + 2 + z + uh(hm)
##      ^ use: a
",
    "1078",
  )
  .check();
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
  .check();
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

#[test]
fn obj_local() {
  JsonnetInput::manifest(
    r#"
{
  local a = 1,
##          ^ def: a
  b: "hi",
  c: a + 2,
##   ^ use: a
}
"#,
    r#"
{
  "b": "hi",
  "c": 3
}
"#,
  )
  .check();
}

#[test]
fn obj_comp_local() {
  JsonnetInput::eval_error(
    r#"
{
  local a = k + "e",
##          ^^^^^^^ def: a
  [k]: std.length(a),
##                ^ use: a
  for k in ["f", "gg"]
}
"#,
    "not yet implemented: makeArray",
  )
  /*
  {
    "f": 2,
    "gg": 3
  }
  */
  .check();
}

#[test]
fn array_comp() {
  JsonnetInput::eval_error(
    r#"
[
  1 + x
##    ^ use: x
  for x in [2, 4]
##    ^ def: x
]
"#,
    "not yet implemented: makeArray",
  )
  .check();
  // [3, 5]
}

#[test]
fn obj_comp_key() {
  JsonnetInput::eval_error(
    r#"
{
## v use: k
  [k]: std.length(k),
##                ^ use: k
  for k in ["a", "bbb"]
##    ^ def: k
}
"#,
    "not yet implemented: makeArray",
  )
  .check();
  /*
  {
    "a": 1,
    "bbb": 3
  }
  */
}

#[test]
fn field_import() {
  Input::default()
    .with_jsonnet(
      "a.libsonnet",
      JsonnetInput::manifest(
        r"
{ foo: 3 }
##     ^ def: foo
",
        r#"
{
  "foo": 3
}
"#,
      ),
    )
    .with_jsonnet(
      "b.jsonnet",
      JsonnetInput::manifest(
        r#"
local b = import "a.libsonnet";
b.foo
##^^^ use: foo
"#,
        "3",
      ),
    )
    .add("b.jsonnet")
    .check();
}

#[test]
fn field_import_assert() {
  Input::default()
    .with_jsonnet(
      "a.libsonnet",
      JsonnetInput::manifest(
        r"
  assert true; { foo: 3 }
##                    ^ def: foo
",
        r#"
{
  "foo": 3
}
"#,
      ),
    )
    .with_jsonnet(
      "b.jsonnet",
      JsonnetInput::manifest(
        r#"
local b = import "a.libsonnet";
b.foo
##^^^ use: foo
"#,
        "3",
      ),
    )
    .add("b.jsonnet")
    .check();
}

#[test]
fn field_local() {
  JsonnetInput::manifest(
    r#"
local foo =
  local bar = 3;
  { quz: bar + 1 };
##       ^^^^^^^ def: quz
foo.quz
##  ^^^ use: quz
"#,
    "4",
  )
  .check();
}

#[test]
fn infinite_1() {
  JsonnetInput::manifest(
    r#"
##    v def: x
local x = x;
##        ^ use: x
0
"#,
    "0",
  )
  .check();
}

// NOTE: not the greatest, but kind of correct ish.
#[test]
fn infinite_2() {
  JsonnetInput::manifest(
    r#"
##    v def: a
local a = b
##        ^ use: a
##    v def: b
    , b = a;
##        ^ use: b
0
"#,
    "0",
  )
  .check();
}

#[test]
fn local_fn_param() {
  JsonnetInput::manifest(
    r"
local f(x) =
##      ^ def: x
  x + 1;
##^ use: x

f(3)
",
    "4",
  )
  .check();
}

#[test]
fn local_fn() {
  JsonnetInput::manifest(
    r"
local f(x) = x + 1;
##           ^^^^^ def: f

    f(2)
##  ^ use: f
",
    "3",
  )
  .check();
}

#[test]
fn obj_plus() {
  JsonnetInput::manifest(
    r#"
local foo = {} + { quz: 3 };
##          ^^^^^^^^^^^^^^^ def: quz
foo.quz
##  ^^^ use: quz
"#,
    r#"
3
"#,
  )
  .check();
}
