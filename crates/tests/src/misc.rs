use crate::check::manifest;

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
