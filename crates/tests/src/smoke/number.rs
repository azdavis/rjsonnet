use crate::check::manifest;
use jsonnet_eval::manifest::Val;
use jsonnet_expr::{Number, Prim};

#[test]
fn int() {
  let want = Val::Prim(Prim::Number(Number::try_from(3.0).unwrap()));
  let got = manifest("3");
  assert_eq!(want, got);
}

#[test]
fn float() {
  let want = Val::Prim(Prim::Number(Number::try_from(3.4).unwrap()));
  let got = manifest("3.4");
  assert_eq!(want, got);
}
