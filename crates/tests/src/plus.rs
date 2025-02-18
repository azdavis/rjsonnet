//! Tests for `+`-related things.

use crate::check::JsonnetInput;

#[test]
fn explicit() {
  JsonnetInput::manifest(
    r"
{ a: 1, b: 2 } + { a: 3, c: 4 }
",
    r#"
{ "a": 3, "b": 2, "c": 4 }
"#,
  )
  .check();
}

#[test]
fn implicit() {
  JsonnetInput::manifest(
    r"
{ a: 1 } { b: 2 }
",
    r#"
{ "a": 1, "b": 2 }
"#,
  )
  .check();
}

#[test]
fn override_field() {
  JsonnetInput::manifest(
    r#"
{
  a: {
    x: 2,
    y: 3,
    z: 4,
  },
} + {
  a: {
    x: 5,
  },
}
"#,
    r#"
{
  "a": {
    "x": 5
  }
}
"#,
  )
  .check();
}

#[test]
fn add_field() {
  JsonnetInput::manifest(
    r#"
{
  a: {
    x: 2,
    y: 3,
    z: 4,
  },
} + {
  a+: {
    x: 5,
  },
}
"#,
    r#"
{
  "a": {
    "x": 5,
    "y": 3,
    "z": 4
  }
}
"#,
  )
  .check();
}

#[test]
fn self_in_field_name_override_field() {
  JsonnetInput::manifest(
    r#"
{
  value: "a",
  o: {
    [self.value]: {
      x: 2,
      y: 3,
      z: 4,
    },
  } + {
    [self.value]: {
      x: 5,
    },
  },
}
"#,
    r#"
{
  "value": "a",
  "o": {
    "a": {
      "x": 5
    }
  }
}
"#,
  )
  .check();
}

#[test]
fn self_in_field_name_add_field() {
  JsonnetInput::manifest(
    r#"
{
  value: "a",
  o: {
    [self.value]: {
      x: 2,
      y: 3,
      z: 4,
    },
  } + {
    [self.value]+: {
      x: 5,
    },
  },
}
"#,
    r#"
{
  "value": "a",
  "o": {
    "a": {
      "x": 5,
      "y": 3,
      "z": 4
    }
  }
}
"#,
  )
  .check();
}

#[test]
fn override_computed_field() {
  JsonnetInput::manifest(
    r#"
local mk(x) = { [x]: null };
local res = { foo: 3 } + mk("foo");
res.foo
##^ type: { foo: any, ... }
"#,
    r#"
null
"#,
  )
  .check();
}

#[test]
fn arrays_ifs() {
  JsonnetInput::manifest(
    r#"
local f(x) =
  assert std.isBoolean(x);
  if x then [1, 2] else [];

f(true) + f(false)
"#,
    r#"[1, 2]"#,
  )
  .check();
}

#[test]
fn any_to_non_addable() {
  JsonnetInput::manifest_or_fn(
    r#"
function(y)
  y + null
##^^^^^^^^ err: invalid use of `+`; expected addable types; left: `any`; right: `null`
"#,
  )
  .check();
}
