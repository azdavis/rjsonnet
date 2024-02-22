//! Tests for numbers, both integral and floating-point.

use crate::check::JsonnetInput;

#[test]
fn int() {
  JsonnetInput::manifest("3", "3.0").check_one();
}

#[test]
fn float() {
  JsonnetInput::manifest("3.4", "3.4").check_one();
}

#[test]
fn dot_exponent_lower() {
  JsonnetInput::manifest("1.2e4", "12000.0").check_one();
}

#[test]
fn dot_exponent_upper() {
  JsonnetInput::manifest("5.6E3", "5600.0").check_one();
}

#[test]
fn exponent_lower_minus() {
  JsonnetInput::manifest("12e-2", "0.12").check_one();
}

#[test]
fn exponent_upper_minus() {
  JsonnetInput::manifest("56E-3", "0.056").check_one();
}

#[test]
fn dot_exponent_lower_plus() {
  JsonnetInput::manifest("1.2e+4", "12000.0").check_one();
}

#[test]
fn dot_exponent_upper_plus() {
  JsonnetInput::manifest("5.6E+3", "5600.0").check_one();
}
