use crate::check::{manifest, manifest_many, manifest_many_add};

#[test]
fn func_arg_id_not_named_arg() {
  manifest(
    r"
local obj = { field: 3 };
local func(x, y) = x;
func(obj.field, 4)
",
    "3",
  );
}

#[test]
#[should_panic = "not yet implemented: std.makeArray"]
fn for_comp_obj() {
  manifest(
    r#"
{
  [x]: false
  for x in ["a", "b"]
}
"#,
    r#"
{
  "a": false,
  "b": false
}
"#,
  );
}

#[test]
fn import_chain() {
  manifest_many_add(
    &[
      ("a.jsonnet", "6 - 5 + 2", "3"),
      ("b.jsonnet", "(import 'a.jsonnet') + 2", "5"),
      ("c.jsonnet", "(import 'b.jsonnet') + 4", "9"),
    ],
    &["c.jsonnet"],
  );
}

#[test]
fn import_self() {
  manifest_many(&[(
    "a.jsonnet",
    r#"
{
  foo: (import "a.jsonnet").bar + 1,
  bar: 3,
}
"#,
    r#"
{
  "foo": 4,
  "bar": 3
}
"#,
  )]);
}
