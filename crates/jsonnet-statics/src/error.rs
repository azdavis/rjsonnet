//! Errors.

use jsonnet_expr::{def, ExprMust, Id, Str, Subst};
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
  pub fn expr_and_def(&self) -> (ExprMust, Option<def::ExprDefKind>) {
    let with_expr = if let Kind::Unused(_, k) = self.kind { Some(k) } else { None };
    (self.expr, with_expr)
  }

  /// Returns the severity of this.
  #[must_use]
  pub fn severity(&self) -> diagnostic::Severity {
    match self.kind {
      Kind::NotInScope(_)
      | Kind::DuplicateFieldName(_)
      | Kind::DuplicateNamedArg(_)
      | Kind::DuplicateBinding(_) => diagnostic::Severity::Error,
      // TODO: make some/all type-checker warnings errors? (once we have more confidence)
      Kind::Unused(_, _)
      | Kind::Incompatible
      | Kind::MissingField
      | Kind::NotEnoughParams
      | Kind::MismatchedParamNames
      | Kind::WantOptionalParamGotRequired
      | Kind::ExtraRequiredParam
      | Kind::AllAlternativesIncompatible
      | Kind::OccursCheck => diagnostic::Severity::Warning,
    }
  }

  /// Apply a subst.
  pub fn apply(&mut self, subst: &Subst) {
    match &mut self.kind {
      Kind::NotInScope(id)
      | Kind::DuplicateNamedArg(id)
      | Kind::DuplicateBinding(id)
      | Kind::Unused(id, _) => {
        id.apply(subst);
      }
      Kind::DuplicateFieldName(str) => str.apply(subst),
      Kind::Incompatible
      | Kind::MissingField
      | Kind::NotEnoughParams
      | Kind::MismatchedParamNames
      | Kind::WantOptionalParamGotRequired
      | Kind::ExtraRequiredParam
      | Kind::AllAlternativesIncompatible
      | Kind::OccursCheck => {}
    }
  }
}

#[derive(Debug)]
pub(crate) enum Kind {
  NotInScope(Id),
  DuplicateFieldName(Str),
  DuplicateNamedArg(Id),
  DuplicateBinding(Id),
  Unused(Id, def::ExprDefKind),
  Incompatible,
  MissingField,
  NotEnoughParams,
  MismatchedParamNames,
  WantOptionalParamGotRequired,
  ExtraRequiredParam,
  AllAlternativesIncompatible,
  OccursCheck,
}

struct Display<'a> {
  kind: &'a Kind,
  ar: &'a jsonnet_expr::StrArena,
}

impl fmt::Display for Display<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match &self.kind {
      Kind::NotInScope(id) => write!(f, "not in scope: `{}`", id.display(self.ar)),
      Kind::DuplicateFieldName(s) => write!(f, "duplicate field name: `{}`", self.ar.get(s)),
      Kind::DuplicateNamedArg(id) => {
        write!(f, "duplicate named argument: `{}`", id.display(self.ar))
      }
      Kind::DuplicateBinding(id) => {
        write!(f, "duplicate binding: `{}`", id.display(self.ar))
      }
      Kind::Unused(id, _) => write!(f, "unused: `{}`", id.display(self.ar)),
      // TODO improve these messages
      Kind::Incompatible => f.write_str("incompatible types"),
      Kind::MissingField => f.write_str("missing field"),
      Kind::NotEnoughParams => f.write_str("not enough parameters"),
      Kind::MismatchedParamNames => f.write_str("mismatched parameter names"),
      Kind::WantOptionalParamGotRequired => {
        f.write_str("wanted an optional parameter, got a required one")
      }
      Kind::ExtraRequiredParam => f.write_str("extra required parameter"),
      Kind::AllAlternativesIncompatible => f.write_str("all union alternatives incompatible"),
      Kind::OccursCheck => f.write_str("occurs check: meta var occurs inside its own solution"),
    }
  }
}
