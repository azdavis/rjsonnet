use crate::check::JsonnetInput;

#[test]
fn bool_true() {
  JsonnetInput::manifest_self("true").check_one();
}

#[test]
fn bool_false() {
  JsonnetInput::manifest_self("false").check_one();
}

#[test]
fn null() {
  JsonnetInput::manifest_self("null").check_one();
}
