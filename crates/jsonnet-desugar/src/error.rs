use std::fmt;

#[derive(Debug)]
pub struct Error {
  pub range: text_size::TextRange,
  pub kind: Kind,
}

#[derive(Debug)]
pub enum Kind {
  CannotRepresentNumber,
  ArrayCompNotOne,
  FirstCompSpecNotFor,
  ObjectCompAssert,
  ObjectCompLiteralFieldName,
  ObjectCompNotOne,
  ObjectCompFieldExtra,
  ObjectCompVisibility,
}

impl fmt::Display for Error {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match &self.kind {
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
    }
  }
}
