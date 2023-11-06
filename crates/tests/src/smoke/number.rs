use crate::check::manifest;

#[test]
fn int() {
  manifest("3", "3.0");
}

#[test]
fn float() {
  manifest("3.4", "3.4");
}

#[test]
fn dot_exponent_lower() {
  manifest("1.2e4", "12000.0");
}

#[test]
fn dot_exponent_upper() {
  manifest("5.6E3", "5600.0");
}

#[test]
fn exponent_lower_minus() {
  manifest("12e-2", "0.12");
}

#[test]
fn exponent_upper_minus() {
  manifest("56E-3", "0.056");
}

#[test]
fn dot_exponent_lower_plus() {
  manifest("1.2e+4", "12000.0");
}

#[test]
fn dot_exponent_upper_plus() {
  manifest("5.6E+3", "5600.0");
}
