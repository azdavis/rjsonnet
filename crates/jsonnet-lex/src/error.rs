//! Errors.

use core::fmt;

/// A lex error.
#[derive(Debug)]
pub struct Error {
  pub(crate) idx: u32,
  pub(crate) kind: Kind,
}

impl Error {
  /// Returns the range of this.
  #[must_use]
  pub fn range(&self) -> text_size::TextRange {
    let ts = text_size::TextSize::new(self.idx);
    text_size::TextRange::empty(ts)
  }
}

#[derive(Debug)]
pub(crate) enum Kind {
  Escape(jsonnet_escape::Error),
  UnclosedComment,
  LeadingZero,
  InvalidBytes,
  NeedDigits,
  InvalidVerbatimDelim,
}

impl fmt::Display for Error {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match &self.kind {
      Kind::Escape(e) => e.fmt(f),
      Kind::UnclosedComment => f.write_str("unclosed comment"),
      Kind::LeadingZero => f.write_str("leading `0` before other digits"),
      Kind::InvalidBytes => f.write_str("invalid bytes"),
      Kind::NeedDigits => f.write_str("need at least one digit"),
      Kind::InvalidVerbatimDelim => f.write_str("invalid verbatim string delimiter"),
    }
  }
}

#[derive(Debug, Default)]
pub(crate) struct Output {
  errors: Vec<Error>,
}

impl Output {
  pub(crate) fn err(&mut self, idx: usize, kind: Kind) {
    self.errors.push(Error { idx: always::convert::usize_to_u32(idx), kind });
  }

  pub(crate) fn finish(self) -> Vec<Error> {
    self.errors
  }
}

impl jsonnet_escape::Output for Output {
  fn err(&mut self, idx: usize, e: jsonnet_escape::Error) {
    self.err(idx, Kind::Escape(e));
  }

  fn byte(&mut self, _: u8) {}
}
