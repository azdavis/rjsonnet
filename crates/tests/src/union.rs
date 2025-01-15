//! Tests for union types.

use crate::check::JsonnetInput;

#[test]
fn more() {
  JsonnetInput::manifest_or_fn(
    r#"
local f(x) =
  assert x == null || std.isString(x) || std.isNumber(x);
  x;
##^ type: null | string | number

function(y)
  assert y == null || std.isString(y);
  f(y)
##  ^ type: null | string
"#,
  )
  .check();
}

#[test]
fn less() {
  JsonnetInput::manifest_or_fn(
    r#"
local f(x) =
  assert x == null || std.isString(x);
  x;
##^ type: null | string

function(y)
  assert y == null || std.isString(y) || std.isNumber(y);
##  v diagnostic: incompatible types; expected `null | string`; found `number`
  f(y)
##  ^ type: null | string | number
"#,
  )
  .check();
}

#[test]
fn obj_more_field() {
  JsonnetInput::manifest_or_fn(
    r#"
local f(x) =
  assert x == null || std.isObject(x);
  x;
##^ type: null | object

function(y)
  assert y == null || (std.isObject(y) && "foo" in y);
  f(y)
##  ^ type: null | { foo: any, ... }
"#,
  )
  .check();
}

#[test]
fn obj_field_better_ty() {
  JsonnetInput::manifest_or_fn(
    r#"
local f(x) =
  assert x == null || (std.isObject(x) && "foo" in x);
  x;
##^ type: null | { foo: any, ... }

function(y)
  assert y == null || (std.isObject(y) && std.isString(y.foo));
  f(y)
##  ^ type: null | { foo: string, ... }
"#,
  )
  .check();
}

#[test]
fn obj_field_worse_ty_any() {
  // allowed because any is top AND bot type (unsafe)
  JsonnetInput::manifest_or_fn(
    r#"
local f(x) =
  assert x == null || (std.isObject(x) && std.isString(x.foo));
  x;
##^ type: null | { foo: string, ... }

function(y)
  assert y == null || (std.isObject(y) && "foo" in y);
  f(y)
##  ^ type: null | { foo: any, ... }
"#,
  )
  .check();
}
