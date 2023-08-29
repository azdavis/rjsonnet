//! The state of the parser.

use drop_bomb::DebugDropBomb;

#[derive(Debug, Default)]
pub(crate) struct St<'a> {
  s: &'a str,
  idx: usize,
  // TODO track locations
  errors: Vec<&'static str>,
}

impl<'a> St<'a> {
  pub(crate) fn new(s: &'a str) -> St<'a> {
    St { s, idx: 0, errors: Vec::new() }
  }

  pub(crate) fn bump(&mut self) {
    self.idx += 1;
  }

  pub(crate) fn cur(&self) -> Option<u8> {
    self.s.as_bytes().get(self.idx).copied()
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

  pub(crate) fn err(&mut self, e: &'static str) {
    self.errors.push(e);
  }

  pub(crate) fn mark(&self) -> Marker {
    Marker { bomb: DebugDropBomb::new("must be passed to `since_mark`"), idx: self.idx }
  }

  pub(crate) fn since_mark(&self, mut m: Marker) -> &'a [u8] {
    m.bomb.defuse();
    assert!(self.idx > m.idx, "failed to advance since marker");
    &self.s.as_bytes()[m.idx..self.idx]
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

  pub(crate) fn finish(self) -> Vec<&'static str> {
    self.errors
  }
}

pub(crate) struct Marker {
  bomb: DebugDropBomb,
  idx: usize,
}
