//! Functions concerning identifiers.

/// Returns whether this is a byte that can start an identifier.
#[must_use]
pub fn is_start(b: u8) -> bool {
  b.is_ascii_alphabetic() || b == b'_'
}

/// Returns whether this is a byte that can continue an identifier (any non-starting byte).
#[must_use]
pub fn is_continue(b: u8) -> bool {
  b.is_ascii_alphanumeric() || b == b'_'
}
