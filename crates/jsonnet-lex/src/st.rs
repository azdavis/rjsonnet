//! The state of the lexer.

use crate::error::{self, Error};
use drop_bomb::DebugDropBomb;
use jsonnet_escape::State;

#[derive(Debug, Default)]
pub(crate) struct St<'a> {
  s: &'a str,
  idx: usize,
  errors: Vec<Error>,
}

impl<'a> St<'a> {
  pub(crate) fn new(s: &'a str) -> St<'a> {
    St { s, idx: 0, errors: Vec::new() }
  }

  pub(crate) fn advance_while<F>(&mut self, f: F)
  where
    F: Fn(u8) -> bool,
  {
    while let Some(b) = self.cur() {
      if f(b) {
        self.bump();
      } else {
        break;
      }
    }
  }

  pub(crate) fn err(&mut self, kind: error::Kind) {
    self.errors.push(Error { idx: self.idx, kind });
  }

  pub(crate) fn mark(&self) -> Marker {
    Marker { bomb: DebugDropBomb::new("must be passed to a `St` method"), idx: self.idx }
  }

  pub(crate) fn non_empty_since(&self, m: Marker) -> &'a [u8] {
    let start = m.idx;
    assert!(self.did_advance_since(m));
    &self.s.as_bytes()[start..self.idx]
  }

  pub(crate) fn did_advance_since(&self, mut m: Marker) -> bool {
    m.bomb.defuse();
    self.idx > m.idx
  }

  pub(crate) fn eat_prefix(&mut self, prefix: &[u8]) -> bool {
    let end = self.idx + prefix.len();
    if self.s.as_bytes().get(self.idx..end).is_some_and(|bs| bs == prefix) {
      self.idx = end;
      true
    } else {
      false
    }
  }

  pub(crate) fn next_str(&mut self) {
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

  pub(crate) fn finish(self) -> Vec<Error> {
    self.errors
  }
}

pub(crate) struct Marker {
  bomb: DebugDropBomb,
  idx: usize,
}

impl<'a> State for St<'a> {
  fn cur(&mut self) -> Option<u8> {
    self.s.as_bytes().get(self.idx).copied()
  }

  fn bump(&mut self) {
    self.idx += 1;
  }

  fn err(&mut self, e: jsonnet_escape::Error) {
    self.err(error::Kind::Escape(e));
  }

  fn output(&mut self, _: u8) {}
}
