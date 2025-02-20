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
  Hole,
  MergeConflictMarker,
  CannotRepresentNumber,
  ArrayCompNotOne,
  FirstCompSpecNotFor,
  ObjectCompAssert,
  ObjectCompLiteralFieldName,
  ObjectCompNotOne,
  ObjectCompFieldParams,
  PathNotFound(String),
  PositionalArgAfterNamedArg,
  ImportTextBlock,
  InvalidSuper,
  Tailstrict,
}

impl fmt::Display for Error {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match &self.kind {
      Kind::Hole => f.write_str("found placeholder hole"),
      Kind::MergeConflictMarker => f.write_str("found Git merge conflict marker"),
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
      Kind::ObjectCompFieldParams => {
        f.write_str("object comprehension field must not have parameters")
      }
      Kind::PathNotFound(p) => write!(f, "path not found: `{p}`"),
      Kind::PositionalArgAfterNamedArg => {
        f.write_str("positional arguments must not appear after named arguments")
      }
      Kind::ImportTextBlock => f.write_str("cannot import a text block"),
      Kind::InvalidSuper => f.write_str("`super` must be used with `.`, `[]`, or `in`"),
      Kind::Tailstrict => f.write_str("`tailstrict` is unstable"),
    }
  }
}
