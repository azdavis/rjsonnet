//! Errors.

use crate::ty;
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
  pub fn display<'a>(
    &'a self,
    store: &'a ty::Store,
    str_ar: &'a jsonnet_expr::StrArena,
  ) -> impl fmt::Display + 'a {
    Display { kind: &self.kind, store, str_ar }
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
      | Kind::Incompatible(_, _)
      | Kind::MissingField(_)
      | Kind::MissingUnknown
      | Kind::NotEnoughParams(_, _)
      | Kind::MismatchedParamNames(_, _)
      | Kind::WantOptionalParamGotRequired(_)
      | Kind::ExtraRequiredParam(_)
      | Kind::Incomparable(_)
      | Kind::MissingArgument(_, _)
      | Kind::ExtraPositionalArgument(_)
      | Kind::ExtraNamedArgument(_)
      | Kind::InvalidPlus(_, _) => diagnostic::Severity::Warning,
    }
  }

  /// Apply a subst.
  pub fn apply(&mut self, subst: &Subst) {
    match &mut self.kind {
      Kind::NotInScope(id)
      | Kind::DuplicateNamedArg(id)
      | Kind::DuplicateBinding(id)
      | Kind::Unused(id, _)
      | Kind::WantOptionalParamGotRequired(id)
      | Kind::ExtraRequiredParam(id)
      | Kind::MissingArgument(id, _)
      | Kind::ExtraNamedArgument(id) => {
        id.apply(subst);
      }
      Kind::DuplicateFieldName(str) | Kind::MissingField(str) => str.apply(subst),
      Kind::MismatchedParamNames(a, b) => {
        a.apply(subst);
        b.apply(subst);
      }
      Kind::Incompatible(_, _)
      | Kind::NotEnoughParams(_, _)
      | Kind::MissingUnknown
      | Kind::Incomparable(_)
      | Kind::ExtraPositionalArgument(_)
      | Kind::InvalidPlus(_, _) => {}
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
  Incompatible(ty::Ty, ty::Ty),
  MissingField(Str),
  /// TODO this is weird.
  MissingUnknown,
  NotEnoughParams(usize, usize),
  MismatchedParamNames(Id, Id),
  WantOptionalParamGotRequired(Id),
  ExtraRequiredParam(Id),
  Incomparable(ty::Ty),
  MissingArgument(Id, ty::Ty),
  ExtraPositionalArgument(usize),
  ExtraNamedArgument(Id),
  InvalidPlus(ty::Ty, ty::Ty),
}

struct Display<'a> {
  kind: &'a Kind,
  store: &'a ty::Store,
  str_ar: &'a jsonnet_expr::StrArena,
}

impl fmt::Display for Display<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match &self.kind {
      Kind::NotInScope(id) => write!(f, "not in scope: `{}`", id.display(self.str_ar)),
      Kind::DuplicateFieldName(s) => write!(f, "duplicate field name: `{}`", self.str_ar.get(s)),
      Kind::DuplicateNamedArg(id) => {
        write!(f, "duplicate named argument: `{}`", id.display(self.str_ar))
      }
      Kind::DuplicateBinding(id) => {
        write!(f, "duplicate binding: `{}`", id.display(self.str_ar))
      }
      Kind::Unused(id, _) => write!(f, "unused: `{}`", id.display(self.str_ar)),
      Kind::Incompatible(want, got) => {
        let want = want.display(self.store, self.str_ar);
        let got = got.display(self.store, self.str_ar);
        f.write_str("incompatible types\n")?;
        writeln!(f, "  expected `{want}`")?;
        write!(f, "     found `{got}`")
      }
      Kind::MissingField(s) => write!(f, "missing field: `{}`", self.str_ar.get(s)),
      Kind::MissingUnknown => f.write_str("missing unknown object fields"),
      Kind::NotEnoughParams(want, got) => {
        f.write_str("not enough parameters\n")?;
        writeln!(f, "  expected at least {want}")?;
        write!(f, "   found only up to {got}")
      }
      Kind::MismatchedParamNames(want, got) => {
        f.write_str("mismatched parameter names\n")?;
        let want = want.display(self.str_ar);
        let got = got.display(self.str_ar);
        writeln!(f, "  expected `{want}`")?;
        write!(f, "     found `{got}`")
      }
      Kind::WantOptionalParamGotRequired(id) => {
        let id = id.display(self.str_ar);
        write!(f, "wanted an optional parameter, got a required one: `{id}`")
      }
      Kind::ExtraRequiredParam(id) => {
        let id = id.display(self.str_ar);
        write!(f, "extra required parameter: `{id}`")
      }
      Kind::Incomparable(ty) => {
        let ty = ty.display(self.store, self.str_ar);
        write!(f, "not a comparable type: `{ty}`")
      }
      Kind::MissingArgument(id, ty) => {
        let id = id.display(self.str_ar);
        let ty = ty.display(self.store, self.str_ar);
        write!(f, "missing argument: `{id}` with type: `{ty}`")
      }
      Kind::ExtraPositionalArgument(n) => write!(f, "extra positional argument: {n}"),
      Kind::ExtraNamedArgument(id) => {
        let id = id.display(self.str_ar);
        write!(f, "extra named argument: `{id}`")
      }
      Kind::InvalidPlus(lhs, rhs) => {
        let lhs = lhs.display(self.store, self.str_ar);
        let rhs = rhs.display(self.store, self.str_ar);
        f.write_str("invalid `+`\n")?;
        writeln!(f, "  left:  `{lhs}`")?;
        write!(f, "  right: `{rhs}`")
      }
    }
  }
}
