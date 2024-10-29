//! Tests for go-to-def.

use crate::check::{Input, JsonnetInput};

// TODO fix, or at least set the def site to the whole +, maybe not the specific rhs. may also need
// to implement infra to have def and use across files in tests
#[test]
#[should_panic = "nothing at def site"]
fn across_file() {
  Input::default()
    .with_jsonnet(
      "foo.jsonnet",
      JsonnetInput::manifest(
        r#"
{} + { quz: 3 }
"#,
        r#"
{ "quz": 3 }
##       ^ def: quz
"#,
      ),
    )
    .with_jsonnet(
      "bar.jsonnet",
      JsonnetInput::manifest(
        r#"
local foo = import 'foo.jsonnet';
foo.quz
##  ^^^ use: quz
"#,
        "3",
      ),
    )
    .add_all()
    .check();
}
