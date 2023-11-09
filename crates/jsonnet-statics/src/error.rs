use jsonnet_expr::{ExprMust, Id, Str};
use std::fmt;

#[derive(Debug)]
pub struct Error {
  pub expr: ExprMust,
  pub kind: Kind,
}

impl Error {
  pub fn display<'a>(&'a self, ar: &'a jsonnet_expr::StrArena) -> impl fmt::Display + 'a {
    Display { kind: &self.kind, ar }
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
  ar: &'a jsonnet_expr::StrArena,
}

impl fmt::Display for Display<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match &self.kind {
      Kind::NotInScope(id) => write!(f, "not in scope: {}", id.display(self.ar)),
      Kind::DuplicateFieldName(s) => write!(f, "duplicate field name: {}", self.ar.get(*s)),
      Kind::DuplicateNamedArg(id) => {
        write!(f, "duplicate named argument: {}", id.display(self.ar))
      }
      Kind::DuplicateBinding(id) => {
        write!(f, "duplicate binding: {}", id.display(self.ar))
      }
    }
  }
}
