//! Tests for the sigs.

use rustc_hash::FxHashSet;

#[test]
fn ok() {
  let mut ident = FxHashSet::<&str>::default();
  let mut content = FxHashSet::<&str>::default();

  for f in super::FNS {
    assert!(ident.insert(f.name.ident()), "duplicate ident: {}", f.name.ident());
    assert!(content.insert(f.name.content()), "duplicate content: {}", f.name.content());

    for example in f.examples.get() {
      let want = format!("std.{}(", f.name.content());
      let ok = example.contains(&want) || (f.name.content() == "format" && example.contains('%'));
      assert!(
        ok,
        "example for `{}` doesn't test the function: {}",
        f.name.content(),
        example.trim(),
      );
    }
  }

  for f in super::FIELDS {
    assert!(ident.insert(f.name.ident()), "duplicate ident: {}", f.name.ident());
    assert!(content.insert(f.name.content()), "duplicate content: {}", f.name.content());
  }
}
