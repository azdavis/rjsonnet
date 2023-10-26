use jsonnet_expr::{ExprMust, Id, Str};
use std::fmt;

#[derive(Debug)]
pub struct Error {
  pub expr: ExprMust,
  pub kind: Kind,
}

impl Error {
  pub fn display(&self) -> impl fmt::Display + '_ {
    Display { kind: &self.kind }
  }
}

#[derive(Debug)]
pub enum Kind {
  NotInScope(Id),
  DuplicateFieldName(Str),
  DuplicateNamedArg(Id),
  DuplicateBinding(Id),
}

struct Display<'a> {
  kind: &'a Kind,
}

impl<'a> fmt::Display for Display<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match &self.kind {
      Kind::NotInScope(_) => f.write_str("not in scope"),
      Kind::DuplicateFieldName(_) => f.write_str("duplicate field name"),
      Kind::DuplicateNamedArg(_) => f.write_str("duplicate named argument"),
      Kind::DuplicateBinding(_) => f.write_str("duplicate binding"),
    }
  }
}
