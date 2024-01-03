use crate::check::manifest_str;

#[test]
#[should_panic = "not yet implemented: std.type"]
fn type_num() {
  manifest_str(
    r"
std.type(3)
",
    "number",
  );
}
