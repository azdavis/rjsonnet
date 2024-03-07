//! Errors.

use std::fmt;

/// An error when desugaring.
#[derive(Debug)]
pub struct Error {
  pub(crate) range: text_size::TextRange,
  pub(crate) kind: Kind,
}

impl Error {
  /// The range of the error.
  #[must_use]
  pub fn range(&self) -> text_size::TextRange {
    self.range
  }
}

#[derive(Debug)]
pub(crate) enum Kind {
  Todo(&'static str),
  CannotRepresentNumber,
  ArrayCompNotOne,
  FirstCompSpecNotFor,
  ObjectCompAssert,
  ObjectCompLiteralFieldName,
  ObjectCompNotOne,
  ObjectCompFieldExtra,
  ObjectCompVisibility,
  PathNotFound(String),
  PositionalArgAfterNamedArg,
}

impl fmt::Display for Error {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match &self.kind {
      Kind::Todo(s) => write!(f, "not yet implemented: {s}"),
      Kind::CannotRepresentNumber => f.write_str("cannot represent number"),
      Kind::ArrayCompNotOne => f.write_str("array comprehension must contain exactly one element"),
      Kind::FirstCompSpecNotFor => {
        f.write_str("the first comprehension specification must be `for`, not `if`")
      }
      Kind::ObjectCompAssert => f.write_str("object comprehension must not contain asserts"),
      Kind::ObjectCompLiteralFieldName => {
        f.write_str("object comprehension must not contain literal field names")
      }
      Kind::ObjectCompNotOne => f.write_str("object comprehension must contain exactly one field"),
      Kind::ObjectCompFieldExtra => {
        f.write_str("object comprehension field must not have `+` or parameters")
      }
      Kind::ObjectCompVisibility => f.write_str("object comprehension field must use `:`"),
      Kind::PathNotFound(p) => write!(f, "path not found: {p}"),
      Kind::PositionalArgAfterNamedArg => {
        write!(f, "positional arguments must not appear after named arguments")
      }
    }
  }
}
