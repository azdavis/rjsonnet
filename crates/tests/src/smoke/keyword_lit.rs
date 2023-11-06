use crate::check::manifest_self;

#[test]
fn bool_true() {
  manifest_self("true");
}

#[test]
fn bool_false() {
  manifest_self("false");
}

#[test]
fn null() {
  manifest_self("null");
}
