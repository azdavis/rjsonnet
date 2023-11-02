use crate::check::{manifest, num};

#[test]
fn int() {
  manifest("3", num(3.0));
}

#[test]
fn float() {
  manifest("3.4", num(3.4));
}
