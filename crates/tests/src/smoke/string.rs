//! TODO figure out how to assert eq for these tests?

use crate::check::manifest;
use jsonnet_eval::manifest::Val;
use jsonnet_expr::Prim;

#[test]
fn double() {
  let got = manifest(
    r#"
"hi"
"#,
  );
  assert!(matches!(got, Val::Prim(Prim::String(_))));
}

#[test]
fn single() {
  let got = manifest(
    r#"
'hi'
"#,
  );
  assert!(matches!(got, Val::Prim(Prim::String(_))));
}

#[test]
fn double_verbatim() {
  let got = manifest(
    r#"
@"hi"
"#,
  );
  assert!(matches!(got, Val::Prim(Prim::String(_))));
}

#[test]
fn single_verbatim() {
  let got = manifest(
    r#"
@'hi'
"#,
  );
  assert!(matches!(got, Val::Prim(Prim::String(_))));
}
