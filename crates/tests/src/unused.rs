//! Unused bindings.

use crate::check::JsonnetInput;

#[test]
fn function_arg() {
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

#[test]
// TODO fix
#[should_panic = "no diagnostic at range"]
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

/// sigh... this doesn't work because there are no real fields to desugar, so the locals get blown
/// away entirely. maybe it's time to not actually desugar away object locals...
#[test]
// TODO fix
#[should_panic = "no diagnostic at range"]
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

#[test]
// TODO fix
#[should_panic = "no diagnostic at range"]
fn array_comp() {
  JsonnetInput::pre_eval_error(
    r#"
[3 for x in []]
##     ^ diagnostic: unused: `x`
"#,
  )
  .check();
}

#[test]
// TODO fix
#[should_panic = "no diagnostic at range"]
fn object_comp() {
  JsonnetInput::pre_eval_error(
    r#"
{
  local no = k,
##      ^^ diagnostic: unused: `no`
  [k]: "hi"
  for k in ["a", "b"]
}
"#,
  )
  .check();
}
