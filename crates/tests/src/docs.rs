//! Tests to make sure the code examples in the docs work.

use crate::check::{markdown, JsonnetInput};
use rustc_hash::FxHashSet;

#[test]
fn tokens() {
  markdown::check(include_str!("../../../docs/tokens.md"));
}

#[test]
fn std_lib() {
  let mut ident = FxHashSet::<&str>::default();
  let mut content = FxHashSet::<&str>::default();
  for f in jsonnet_std_sig::FNS {
    assert!(ident.insert(f.name.ident()), "duplicate ident {}", f.name.ident());
    assert!(content.insert(f.name.content()), "duplicate content {}", f.name.content());
    if !f.implemented {
      continue;
    }
    markdown::check(f.doc);
    for example in f.examples {
      JsonnetInput::manifest(example, "true").check();
    }
  }
}
