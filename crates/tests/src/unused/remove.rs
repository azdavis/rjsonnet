//! Removing unused locals.

use crate::check::JsonnetInput;
use jsonnet_analyze::remove;

#[test]
fn smoke() {
  JsonnetInput::rm_unused(
    r#"
local x = 1;
2
"#,
    r#"
2
"#,
  )
  .check();
}

#[test]
fn multi_binding_fst() {
  JsonnetInput::rm_unused(
    r#"
local x = 1, y = 2;
y
"#,
    r#"
local y = 2;
y
"#,
  )
  .check();
}

#[test]
fn multi_binding_snd() {
  JsonnetInput::rm_unused(
    r#"
local x = 1, y = 2;
x
"#,
    r#"
local x = 1;
x
"#,
  )
  .check();
}

#[test]
fn multi_binding_all() {
  JsonnetInput::rm_unused(
    r#"
local x = 1, y = 2;
3
"#,
    r#"
3
"#,
  )
  .check();
}

#[test]
fn surrounded() {
  JsonnetInput::rm_unused(
    r#"
local a = 1;
local b = 2;
local c = 3;

a + c
"#,
    r#"
local a = 1;
local c = 3;

a + c
"#,
  )
  .check();
}

#[test]
fn slash_comment_directly_above() {
  JsonnetInput::rm_unused(
    r#"
// foo
local used = 1;

// bar
local unused = 2;

used + 1
"#,
    r#"
// foo
local used = 1;




used + 1
"#,
  )
  .check();
}

#[test]
fn slash_comment_directly_below() {
  JsonnetInput::rm_unused(
    r#"
local used = 1;
// foo

local unused = 2;
// bar

used + 1
"#,
    r#"
local used = 1;
// foo




used + 1
"#,
  )
  .check();
}

#[test]
fn block_comment_directly_above() {
  JsonnetInput::rm_unused(
    r#"
/*
 * foo bar
 * quz
 */
local used = 1;

/*
 * beep bop
 * blip
 */
local unused = 2;

used + 1
"#,
    r#"
/*
 * foo bar
 * quz
 */
local used = 1;




used + 1
"#,
  )
  .check();
}

#[test]
fn block_comment_directly_below() {
  JsonnetInput::rm_unused(
    r#"
local used = 1;
/*
 * foo bar
 * quz
 */

local unused = 2;
/*
 * beep bop
 * blip
 */

used + 1
"#,
    r#"
local used = 1;
/*
 * foo bar
 * quz
 */




used + 1
"#,
  )
  .check();
}

#[test]
fn slash_comment_not_directly_above() {
  JsonnetInput::rm_unused(
    r#"
// foo

local bar = 2;

4
"#,
    r#"
// foo



4
"#,
  )
  .check();
}

#[test]
fn slash_comment_not_directly_below() {
  JsonnetInput::rm_unused(
    r#"
local bar = 2;

// foo

4
"#,
    r#"


// foo

4
"#,
  )
  .check();
}

#[test]
fn block_comment_not_directly_above() {
  JsonnetInput::rm_unused(
    r#"
/* foo */

local bar = 2;

4
"#,
    r#"
/* foo */



4
"#,
  )
  .check();
}

#[test]
fn block_comment_not_directly_below() {
  JsonnetInput::rm_unused(
    r#"
local bar = 2;

/* foo */

4
"#,
    r#"


/* foo */

4
"#,
  )
  .check();
}

/// NOTE may not be great, since the comment quz may be related to local blob
#[test]
fn slash_comment_directly_above_and_below() {
  JsonnetInput::rm_unused(
    r#"
// foo
local bar = 1;
// quz
local blob = 2;
// hi
blob + 1
"#,
    r#"
local blob = 2;
// hi
blob + 1
"#,
  )
  .check();
}

#[test]
fn only_above() {
  let opts = remove::Options {
    flavor: remove::Flavor::All,
    comments: remove::Comments { above: true, below: false },
  };
  JsonnetInput::rm_unused_with(
    opts,
    r#"
// foo
local bar = 1;
// quz
local blob = 2;
// hi
blob + 1
"#,
    r#"
// quz
local blob = 2;
// hi
blob + 1
"#,
  )
  .check();
}

#[test]
fn only_below() {
  let opts = remove::Options {
    flavor: remove::Flavor::All,
    comments: remove::Comments { above: false, below: true },
  };
  JsonnetInput::rm_unused_with(
    opts,
    r#"
// foo
local bar = 1;
// quz
local blob = 2;
// hi
blob + 1
"#,
    r#"
// foo
local blob = 2;
// hi
blob + 1
"#,
  )
  .check();
}

#[test]
#[should_panic = "should remove unused"]
fn obj() {
  JsonnetInput::rm_unused(
    r#"
{
  local x = 1,
  a: 2,
}
"#,
    r#"
{
  a: 2,
}
"#,
  )
  .check();
}
