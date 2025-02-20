//! Tests for `+`-related things with objects, like `+` between objects and `+` in fields.

use crate::check::JsonnetInput;

#[test]
fn both_default() {
  JsonnetInput::manifest(
    r"
{ a: 1 } + { a: 2 }
",
    r#"
{ "a": 2 }
"#,
  )
  .check();
}

#[test]
#[should_panic = "mismatched manifest"]
fn lhs_hidden() {
  JsonnetInput::manifest(
    r"
{ a:: 1 } + { a: 2 }
",
    r#"
{}
"#,
  )
  .check();
}

#[test]
fn rhs_hidden() {
  JsonnetInput::manifest(
    r"
{ a: 1 } + { a:: 2 }
",
    r#"
{}
"#,
  )
  .check();
}

#[test]
fn lhs_hidden_rhs_visible() {
  JsonnetInput::manifest(
    r"
{ a:: 1 } + { a::: 2 }
",
    r#"
{ "a": 2 }
"#,
  )
  .check();
}

#[test]
fn lhs_visible_rhs_hidden() {
  JsonnetInput::manifest(
    r"
{ a::: 1 } + { a:: 2 }
",
    r#"
{}
"#,
  )
  .check();
}

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
