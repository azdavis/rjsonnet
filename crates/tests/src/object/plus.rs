//! Tests for `+`-related things in objects.

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
#[should_panic = "mismatched manifest"]
fn add_field() {
  // TODO impl support for +:
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
  foo: "a",
  inner: {
    [self.foo]: {
      x: 2,
      y: 3,
      z: 4,
    },
  } + {
    [self.foo]: {
      x: 5,
    },
  },
}
"#,
    r#"
{
  "foo": "a",
  "inner": {
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
#[should_panic = "mismatched manifest"]
fn self_in_field_name_add_field() {
  // TODO impl support for +:
  JsonnetInput::manifest(
    r#"
{
  foo: "a",
  inner: {
    [self.foo]: {
      x: 2,
      y: 3,
      z: 4,
    },
  } + {
    [self.foo]+: {
      x: 5,
    },
  },
}
"#,
    r#"
{
  "foo": "a",
  "inner": {
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
