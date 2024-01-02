use crate::check::manifest_str;

#[test]
#[should_panic = "no such field: type"]
fn type_num() {
  manifest_str(
    r"
std.type(3)
",
    "number",
  );
}
