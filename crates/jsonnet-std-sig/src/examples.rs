//! See [`Examples`].

/// A container of examples.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Examples(&'static [&'static str]);

impl Examples {
  /// The min number of examples for a non-empty one.
  pub const MIN: usize = 2;

  /// Returns a new examples. Requires the length be at least MIN.
  #[must_use]
  pub const fn new(xs: &'static [&'static str]) -> Self {
    assert!(xs.len() >= Self::MIN);
    Self(xs)
  }

  /// An empty examples that is allowed since this function can't have examples, has
  /// non-standard-ly-formatted examples, or is deprecated.
  pub const EMPTY_ALLOWED: Self = Self(&[]);

  /// An empty examples that should eventually be turned into a non-empty examples.
  pub const EMPTY_TODO: Self = Self(&[]);

  /// Returns whether this is empty.
  #[must_use]
  pub const fn is_empty(self) -> bool {
    self.0.is_empty()
  }

  /// Returns the examples.
  #[must_use]
  pub const fn get(self) -> &'static [&'static str] {
    self.0
  }
}
