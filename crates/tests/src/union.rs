//! Tests for union types.

use crate::check::JsonnetInput;

#[test]
fn specific() {
  JsonnetInput::manifest_or_fn(
    r#"
local f(x) =
  assert x == null || std.isNumber(x) || std.isString(x);
  x;
##^ type: null | number | string

function(y)
  assert y == null || std.isString(y);
  f(y)
##  ^ type: null | string
"#,
  )
  .check();
}

#[test]
fn permissive() {
  JsonnetInput::manifest_or_fn(
    r#"
local f(x) =
  assert x == null || std.isString(x);
  x;
##^ type: null | string

function(y)
  assert y == null || std.isNumber(y) || std.isString(y);
##  v diagnostic: incompatible types; expected `null | string`; found `number`
  f(y)
##  ^ type: null | number | string
"#,
  )
  .check();
}

#[test]
fn obj_specific_field() {
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
fn obj_field_specific_ty() {
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
fn obj_field_permissive_ty() {
  // NOTE the never is not great, not terrible
  JsonnetInput::manifest_or_fn(
    r#"
local f(x) =
  assert x == null || (std.isObject(x) && std.isString(x.foo));
  x;
##^ type: null | { foo: string, ... }

function(y)
  assert y == null || (std.isObject(y) && (std.isString(y.foo) || std.isNumber(y.foo)));
##  v diagnostic: incompatible types; expected `null | { foo: string, ... }`; found `{ foo: number, ... }`
  f(y)
##  ^ type: null | { foo: string, ... } | { foo: number, ... } | { foo: never, ... }
"#,
  )
  .check();
}

#[test]
fn obj_field_permissive_any() {
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
