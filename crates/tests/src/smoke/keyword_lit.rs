use crate::check::manifest;
use jsonnet_eval::manifest::Val;
use jsonnet_expr::Prim;

#[test]
fn bool_true() {
  let want = Val::Prim(Prim::Bool(true));
  let got = manifest("true");
  assert_eq!(want, got);
}

#[test]
fn bool_false() {
  let want = Val::Prim(Prim::Bool(false));
  let got = manifest("false");
  assert_eq!(want, got);
}

#[test]
fn null() {
  let want = Val::Prim(Prim::Null);
  let got = manifest("null");
  assert_eq!(want, got);
}
