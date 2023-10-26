use core::fmt;

#[derive(Debug)]
pub struct Error {
  pub idx: usize,
  pub kind: Kind,
}

#[derive(Debug)]
pub enum Kind {
  Escape(jsonnet_escape::Error),
  UnclosedComment,
  LeadingZero,
  InvalidBytes,
  NeedDigits,
}

impl fmt::Display for Error {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match &self.kind {
      Kind::Escape(e) => e.fmt(f),
      Kind::UnclosedComment => f.write_str("unclosed comment"),
      Kind::LeadingZero => f.write_str("leading `0` before other digits"),
      Kind::InvalidBytes => f.write_str("invalid bytes"),
      Kind::NeedDigits => f.write_str("need at least one digit"),
    }
  }
}
