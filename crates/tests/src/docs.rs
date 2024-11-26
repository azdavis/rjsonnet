//! Tests to make sure the code examples in the docs work.

use crate::check::markdown::check;

#[test]
fn tokens() {
  check(include_str!("../../../docs/tokens.md"));
}

#[test]
fn std_lib() {
  check(include_str!("../../../docs/std_lib.md"));
}

#[test]
fn std_lib_new() {
  for f in jsonnet_std_sig::FNS {
    check(f.doc);
  }
}
