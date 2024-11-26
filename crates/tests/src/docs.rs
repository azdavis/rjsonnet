//! Tests to make sure the code examples in the docs work.

use crate::check::{markdown, JsonnetInput};

#[test]
fn tokens() {
  markdown::check(include_str!("../../../docs/tokens.md"));
}

#[test]
fn std_lib() {
  for f in jsonnet_std_sig::FNS {
    if !f.implemented {
      continue;
    }
    markdown::check(f.doc);
    for example in f.examples {
      JsonnetInput::manifest(example, "true").check();
    }
  }
}
