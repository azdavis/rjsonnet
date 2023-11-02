use crate::check::manifest;
use jsonnet_eval::manifest::Val;
use jsonnet_expr::Prim;

#[test]
fn bool_true() {
  manifest("true", Val::Prim(Prim::Bool(true)));
}

#[test]
fn bool_false() {
  manifest("false", Val::Prim(Prim::Bool(false)));
}

#[test]
fn null() {
  manifest("null", Val::Prim(Prim::Null));
}
