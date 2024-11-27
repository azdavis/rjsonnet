//! Tests for the sigs.

use rustc_hash::FxHashSet;

#[test]
fn ok() {
  let mut text = FxHashSet::<&str>::default();

  for tok in super::ALL {
    assert!(text.insert(tok.text), "duplicate token: {}", tok.text);
    assert!(!tok.purposes.is_empty(), "no purpose: {}", tok.text);

    for purpose in tok.purposes {
      assert!(
        purpose.example.contains(tok.text),
        "example for `{}` doesn't showcase the token: {}",
        tok.text,
        purpose.example,
      );
    }
  }
}
