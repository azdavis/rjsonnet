//! Process a string.

#![deny(clippy::pedantic, missing_debug_implementations, missing_docs, rust_2018_idioms)]

use drop_bomb::DebugDropBomb;

/// The state for processing a string.
#[derive(Debug, Default)]
pub struct St<'a> {
  s: &'a str,
  idx: usize,
}

impl<'a> St<'a> {
  /// Returns a new state for the string.
  #[must_use]
  pub fn new(s: &'a str) -> St<'a> {
    St { s, idx: 0 }
  }

  /// Returns the current byte.
  #[must_use]
  pub fn cur(&self) -> Option<u8> {
    self.s.as_bytes().get(self.idx).copied()
  }

  /// Returns the current byte index. Use this for marking where errors occur in the string.
  #[must_use]
  pub fn cur_idx(&self) -> usize {
    self.idx
  }

  /// Advances the index by 1.
  pub fn bump(&mut self) {
    self.idx += 1;
  }

  /// Advances the index while `cond` holds true.
  pub fn bump_while<F>(&mut self, mut cond: F)
  where
    F: FnMut(u8) -> bool,
  {
    while let Some(b) = self.cur() {
      if cond(b) {
        self.bump();
      } else {
        break;
      }
    }
  }

  /// Returns a marker that must be consumed later.
  #[must_use]
  pub fn mark(&self) -> Marker {
    Marker { bomb: DebugDropBomb::new("must be passed to a `St` method"), idx: self.idx }
  }

  /// Returns a non-empty slice since the marker.
  ///
  /// # Panics
  ///
  /// If it would return an empty slice.
  #[must_use]
  pub fn non_empty_since(&self, m: Marker) -> &'a [u8] {
    let start = m.idx;
    assert!(self.did_bump_since(m));
    &self.s.as_bytes()[start..self.idx]
  }

  /// Returns the slice since the marker.
  ///
  /// NOTE: allowed to return an empty slice.
  #[must_use]
  pub fn since(&self, mut m: Marker) -> &'a [u8] {
    let start = m.idx;
    m.bomb.defuse();
    &self.s.as_bytes()[start..self.idx]
  }

  /// Returns whether the state was bumped since the marker.
  #[must_use]
  pub fn did_bump_since(&self, mut m: Marker) -> bool {
    m.bomb.defuse();
    self.idx > m.idx
  }

  /// If the next few bytes of the string are equal to prefix, advance by that much and return true.
  /// Else return false.
  pub fn eat_prefix(&mut self, prefix: &[u8]) -> bool {
    let end = self.idx + prefix.len();
    if self.s.as_bytes().get(self.idx..end).is_some_and(|bs| bs == prefix) {
      self.idx = end;
      true
    } else {
      false
    }
  }

  /// Advances the index to the next char boundary.
  pub fn next_str(&mut self) {
    self.bump();
    loop {
      if self.s.is_char_boundary(self.idx) {
        break;
      }
      match self.cur() {
        Some(_) => self.bump(),
        None => unreachable!("got to the end without a valid str"),
      }
    }
  }
}

/// A marker for the current position.
#[derive(Debug)]
pub struct Marker {
  bomb: DebugDropBomb,
  idx: usize,
}
