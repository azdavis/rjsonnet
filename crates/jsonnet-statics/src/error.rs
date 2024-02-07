//! Errors.

use jsonnet_expr::{ExprMust, Id, Str, Subst};
use std::fmt;

/// An error.
#[derive(Debug)]
pub struct Error {
  pub(crate) expr: ExprMust,
  pub(crate) kind: Kind,
}

impl Error {
  /// Display the error.
  #[must_use]
  pub fn display<'a>(&'a self, ar: &'a jsonnet_expr::StrArena) -> impl fmt::Display + 'a {
    Display { kind: &self.kind, ar }
  }

  /// Returns the expr this error is for.
  #[must_use]
  pub fn expr(&self) -> ExprMust {
    self.expr
  }

  /// Apply a subst.
  pub fn apply(&mut self, subst: &Subst) {
    match &mut self.kind {
      Kind::NotInScope(id) | Kind::DuplicateNamedArg(id) | Kind::DuplicateBinding(id) => {
        id.apply(subst);
      }
      Kind::DuplicateFieldName(str) => str.apply(subst),
    }
  }
}

#[derive(Debug)]
pub(crate) enum Kind {
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
      Kind::DuplicateFieldName(s) => write!(f, "duplicate field name: {}", self.ar.get(s)),
      Kind::DuplicateNamedArg(id) => {
        write!(f, "duplicate named argument: {}", id.display(self.ar))
      }
      Kind::DuplicateBinding(id) => {
        write!(f, "duplicate binding: {}", id.display(self.ar))
      }
    }
  }
}
