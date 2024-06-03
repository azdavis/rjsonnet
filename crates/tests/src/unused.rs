//! Unused bindings.

use crate::check::JsonnetInput;

#[test]
fn fn_param() {
  JsonnetInput::pre_eval_error(
    r"
local uh = function(x) 3;
##                  ^ diagnostic: unused: `x`
uh(4)
",
  )
  .check();
}

#[test]
fn local() {
  JsonnetInput::pre_eval_error(
    r"
local y = 3;
##    ^ diagnostic: unused: `y`
{}
",
  )
  .check();
}

/// TODO tighten range
#[test]
fn local_param() {
  JsonnetInput::pre_eval_error(
    r"
local f(x) = 3;
##     ^^^ diagnostic: unused: `x`
f(0)
",
  )
  .check();
}

#[test]
fn in_all_fields() {
  JsonnetInput::pre_eval_error(
    r#"
{
  local a = 1,
##      ^ diagnostic: unused: `a`
  b: "hi",
  c: 3,
}
"#,
  )
  .check();
}

#[test]
fn no_field() {
  JsonnetInput::pre_eval_error(
    r#"
{
  local a = 1,
##      ^ diagnostic: unused: `a`
}
"#,
  )
  .check();
}

/// TODO tighten range
#[test]
fn array_comp() {
  JsonnetInput::pre_eval_error(
    r#"
[3 for x in []]
## ^^^^^^^^^^^ diagnostic: unused: `x`
"#,
  )
  .check();
}

#[test]
fn object_comp() {
  JsonnetInput::pre_eval_error(
    r#"
{
  local no = k,
  [k]: false,
##^^^^^^^^^^ diagnostic: unused: `no`
  for k in ["a", "b"]
}
"#,
  )
  .check();
}
