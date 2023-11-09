use core::fmt;

/// A lex error.
#[derive(Debug)]
pub struct Error {
  /// TODO replace with a text range?
  #[allow(dead_code)]
  pub(crate) idx: usize,
  pub(crate) kind: Kind,
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
