use jsonnet_expr::{ExprMust, Id, Str};
use std::fmt;

#[derive(Debug)]
pub struct Error {
  pub expr: ExprMust,
  pub kind: Kind,
}

impl Error {
  pub fn display<'a>(&'a self, str_arena: &'a jsonnet_expr::StrArena) -> impl fmt::Display + 'a {
    Display { kind: &self.kind, str_arena }
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
  str_arena: &'a jsonnet_expr::StrArena,
}

impl<'a> fmt::Display for Display<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match &self.kind {
      Kind::NotInScope(id) => write!(f, "not in scope: {}", self.str_arena.get(id.inner())),
      Kind::DuplicateFieldName(s) => write!(f, "duplicate field name: {}", self.str_arena.get(*s)),
      Kind::DuplicateNamedArg(id) => {
        write!(f, "duplicate named argument: {}", self.str_arena.get(id.inner()))
      }
      Kind::DuplicateBinding(id) => {
        write!(f, "duplicate binding: {}", self.str_arena.get(id.inner()))
      }
    }
  }
}
