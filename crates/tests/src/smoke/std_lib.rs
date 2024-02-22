use crate::check::JsonnetInput;

#[test]
#[should_panic = "not yet implemented: std.type"]
fn type_num() {
  JsonnetInput::string(
    r"
std.type(3)
",
    "number",
  )
  .check_one();
}
