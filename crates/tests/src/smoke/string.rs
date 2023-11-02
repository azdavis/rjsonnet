use crate::check::manifest_raw;
use jsonnet_eval::manifest::Val;
use jsonnet_expr::Prim;

fn check(prog: &str, want: &str) {
  let (mut desugar, got) = manifest_raw(prog);
  let want = desugar.arenas.str.insert(want.to_owned().into_boxed_str());
  assert_eq!(got, Val::Prim(Prim::String(want)));
}

#[test]
fn double() {
  check(
    r#"
"hi"
"#,
    "hi",
  );
}

#[test]
fn single() {
  check("'hi'", "hi");
}

#[test]
fn double_escape() {
  check(
    r#"
"hi\nthere\"my'friend\'buddy"
"#,
    "hi\nthere\"my'friend'buddy",
  );
}

#[test]
fn single_escape() {
  check(
    r#"
'hi\nthere"my\'friend\"buddy'
"#,
    "hi\nthere\"my'friend\"buddy",
  );
}

#[test]
fn double_verbatim() {
  check(
    r#"
@"hi"
"#,
    "hi",
  );
}

#[test]
fn single_verbatim() {
  check(
    r#"
@'hi'
"#,
    "hi",
  );
}

#[test]
fn double_verbatim_escape() {
  check(
    r#"
@"hi "" '' \\ \n there"
"#,
    r#"hi " '' \\ \n there"#,
  );
}

#[test]
#[should_panic = "lex error: unclosed string"]
fn unclosed() {
  check("'", "");
}
