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

/// Returns whether this is an ident.
#[must_use]
pub fn is(s: &str) -> bool {
  s.as_bytes()
    .split_first()
    .is_some_and(|(&fst, rest)| is_start(fst) && rest.iter().copied().all(is_continue))
}
