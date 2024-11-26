//! Tests for the sigs.

use rustc_hash::FxHashSet;

#[test]
fn no_dupe() {
  let mut ident = FxHashSet::<&str>::default();
  let mut content = FxHashSet::<&str>::default();
  for f in super::FNS {
    assert!(ident.insert(f.name.ident()), "duplicate ident: {}", f.name.ident());
    assert!(content.insert(f.name.content()), "duplicate content: {}", f.name.content());
  }
}
