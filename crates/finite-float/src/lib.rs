//! See [`Float`].

use always::always;
use std::fmt;

/// A finite floating-point number, that is, one that is not NaN or infinity.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Float(f64);

impl Float {
  /// 0
  pub const POSITIVE_ZERO: Self = Self(0.0);

  /// 1
  pub const POSITIVE_ONE: Self = Self(1.0);

  /// -1
  pub const NEGATIVE_ONE: Self = Self(-1.0);

  /// π
  pub const PI: Self = Self(std::f64::consts::PI);

  /// Exposes the inner value of this number. It will be finite.
  #[must_use]
  pub fn value(&self) -> f64 {
    self.0
  }

  /// Delegates to `try_from`, and uses always! to assert the Err case is not hit. But if it is, use
  /// `0.0` instead.
  #[must_use]
  pub fn always_from_f64(n: f64) -> Self {
    match Self::try_from(n) {
      Ok(n) => n,
      Err(e) => {
        always!(false, "infinite: {e}");
        Self(0.0)
      }
    }
  }
}

/// OK because NaN is not allowed
impl Eq for Float {}

impl PartialOrd for Float {
  fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
    Some(self.cmp(other))
  }
}

/// OK because NaN is not allowed
impl Ord for Float {
  fn cmp(&self, other: &Self) -> std::cmp::Ordering {
    if let Some(x) = self.value().partial_cmp(&other.value()) {
      x
    } else {
      always!(false, "should not be NaN");
      std::cmp::Ordering::Equal
    }
  }
}

/// OK because NaN is not allowed
impl std::hash::Hash for Float {
  fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
    state.write_u64(self.0.to_bits());
  }
}

impl TryFrom<f64> for Float {
  type Error = Infinite;

  fn try_from(value: f64) -> Result<Self, Self::Error> {
    if value.is_nan() {
      return Err(Infinite::Nan);
    }
    if value.is_infinite() {
      let inf = if value.is_sign_positive() { Infinite::Pos } else { Infinite::Neg };
      return Err(inf);
    }
    Ok(Self(value))
  }
}

impl From<usize> for Float {
  fn from(value: usize) -> Self {
    #[expect(clippy::cast_precision_loss)]
    Self(value as f64)
  }
}

impl From<u8> for Float {
  fn from(value: u8) -> Self {
    Self(value.into())
  }
}

impl std::ops::Neg for Float {
  type Output = Self;

  fn neg(self) -> Self::Output {
    Self(-self.0)
  }
}

impl fmt::Display for Float {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    self.0.fmt(f)
  }
}

/// A way to be not finite.
#[derive(Debug, Clone, Copy)]
pub enum Infinite {
  /// Not a number.
  Nan,
  /// Positive infinity.
  Pos,
  /// Negative infinity.
  Neg,
}

impl fmt::Display for Infinite {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Infinite::Nan => f.write_str("not a number"),
      Infinite::Pos => f.write_str("positive infinity"),
      Infinite::Neg => f.write_str("negative infinity"),
    }
  }
}
