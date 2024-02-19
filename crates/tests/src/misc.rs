use crate::check::manifest;

// TODO fix
#[test]
#[should_panic = "expected `)`"]
fn t1() {
  manifest(
    r"
local obj = { field: 3 };
local func(x, y) = x;
func(obj.field, 4)
",
    "3",
  );
}
