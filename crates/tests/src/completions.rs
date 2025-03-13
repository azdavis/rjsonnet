//! Tests for completions.

use crate::check::JsonnetInput;

#[test]
fn smoke() {
  JsonnetInput::manifest_or_fn(
    r#"
local obj = { a: 1, b: null, c: "hi" };
obj.a
##  ^ completions: a: number; b: null; c: string
"#,
  )
  .check();
}

#[test]
fn with_syntax_error() {
  JsonnetInput::pre_eval_error(
    r#"
local obj = { a: 1, b: null, c: "hi" };
##              v completions: a: number; b: null; c: string
local res = obj.   ;
##                 ^ err: expected an identifier, found `;`
res
"#,
  )
  .check();
}

#[test]
fn union_inside() {
  JsonnetInput::manifest_or_fn(
    r#"
function(b)
  assert std.isBoolean(b);
  local obj = { a: if b then 1 else "hi" };
  obj.a
##    ^ completions: a: number | string
"#,
  )
  .check();
}

#[test]
fn union_outside() {
  JsonnetInput::manifest_or_fn(
    r#"
function(b)
  assert std.isBoolean(b);
  local obj = if b then { a: 1 } else { a: "hi" };
  obj.a
##    ^ completions: a: number | string
"#,
  )
  .check();
}

#[test]
fn union_inside_inside() {
  JsonnetInput::manifest_or_fn(
    r#"
function(x, y, z)
  assert std.isBoolean(x);
  assert std.isBoolean(y);
  assert std.isBoolean(z);
  local obj = {
    a:
      if x then
        { b: if y then 1 else false }
      else
        { b: if z then true else "hi" }
  };
  obj.a.b
##      ^ completions: b: boolean | number | string
"#,
  )
  .check();
}

#[test]
fn union_inside_outside() {
  JsonnetInput::manifest_or_fn(
    r#"
function(x, y, z)
  assert std.isBoolean(x);
  assert std.isBoolean(y);
  assert std.isBoolean(z);
  local obj = {
    a:
      if x then
        if y then
          { b: 1 }
        else
          { b: false }
      else
        if z then
          { b: true }
        else
          { b: "hi" }
  };
  obj.a.b
##      ^ completions: b: boolean | number | string
"#,
  )
  .check();
}

#[test]
fn union_outside_inside() {
  JsonnetInput::manifest_or_fn(
    r#"
function(x, y, z)
  assert std.isBoolean(x);
  assert std.isBoolean(y);
  assert std.isBoolean(z);
  local obj =
    if x then
      {
        a: { b: if y then 1 else false }
      }
    else
      {
        a: { b: if z then true else "hi" }
      }
  ;
  obj.a.b
##      ^ completions: b: boolean | number | string
"#,
  )
  .check();
}

#[test]
fn union_outside_outside() {
  JsonnetInput::manifest_or_fn(
    r#"
function(x, y, z)
  assert std.isBoolean(x);
  assert std.isBoolean(y);
  assert std.isBoolean(z);
  local obj =
    if x then
      {
        a:
          if y then
            { b: 1 }
          else
            { b: false }
      }
    else
      {
        a:
          if z then
            { b: true }
          else
            { b: "hi" }
      }
  ;
  obj.a.b
##      ^ completions: b: boolean | number | string
"#,
  )
  .check();
}
