//! Static errors.

use jsonnet_expr::{def, ExprMust, Id, Str};
use jsonnet_ty as ty;
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
    multi_line: ty::display::MultiLine,
    store: &'a ty::GlobalStore,
    str_ar: &'a jsonnet_expr::StrArena,
  ) -> impl fmt::Display + 'a {
    Display { multi_line, kind: &self.kind, store, str_ar }
  }

  /// Returns the expr this error is for.
  #[must_use]
  pub fn expr_and_def(&self) -> (ExprMust, Option<def::ExprDefKind>) {
    let with_expr = match self.kind {
      Kind::Unused(_, k) => Some(k),
      Kind::DuplicateBinding(_, n, m) => Some(def::ExprDefKind::Multi(n, m)),
      _ => None,
    };
    (self.expr, with_expr)
  }

  /// Returns the severity of this.
  #[must_use]
  pub fn severity(&self) -> diagnostic::Severity {
    match self.kind {
      Kind::NotInScope(_)
      | Kind::DuplicateFieldName(_)
      | Kind::DuplicateNamedArg(_)
      | Kind::DuplicateBinding(_, _, _) => diagnostic::Severity::Error,
      // TODO: make some/all type-checker warnings errors? (once we have more confidence)
      Kind::Unused(_, _)
      | Kind::Unify(_)
      | Kind::Incomparable(_)
      | Kind::MissingArgument(_, _)
      | Kind::ExtraPositionalArgument(_)
      | Kind::ExtraNamedArgument(_)
      | Kind::InvalidPlus(_, _)
      | Kind::CallNonFn(_)
      | Kind::InvalidLength(_)
      | Kind::InvalidSubscript(_) => diagnostic::Severity::Warning,
    }
  }

  /// Apply a substitution.
  ///
  /// NOTE: no need to apply an expr subst because we run statics after combining the per-file
  /// syntax artifacts.
  pub fn apply(&mut self, ty_subst: &ty::Subst) {
    match &mut self.kind {
      Kind::MissingArgument(_, ty)
      | Kind::Incomparable(ty)
      | Kind::CallNonFn(ty)
      | Kind::InvalidLength(ty)
      | Kind::InvalidSubscript(ty) => {
        ty.apply(ty_subst);
      }
      Kind::Unify(Unify::Incompatible(a, b)) | Kind::InvalidPlus(a, b) => {
        a.apply(ty_subst);
        b.apply(ty_subst);
      }
      Kind::NotInScope(_)
      | Kind::DuplicateNamedArg(_)
      | Kind::DuplicateBinding(_, _, _)
      | Kind::Unused(_, _)
      | Kind::ExtraNamedArgument(_)
      | Kind::DuplicateFieldName(_)
      | Kind::ExtraPositionalArgument(_)
      | Kind::Unify(
        Unify::WantOptionalParamGotRequired(_)
        | Unify::ExtraRequiredParam(_)
        | Unify::MissingField(_)
        | Unify::MismatchedParamNames(_, _)
        | Unify::NotEnoughParams(_, _),
      ) => {}
    }
  }
}

/// Errors specific to unification.
#[derive(Debug)]
pub(crate) enum Unify {
  Incompatible(ty::Ty, ty::Ty),
  MissingField(Str),
  NotEnoughParams(usize, usize),
  MismatchedParamNames(Id, Id),
  WantOptionalParamGotRequired(Id),
  ExtraRequiredParam(Id),
}

#[derive(Debug)]
pub(crate) enum Kind {
  NotInScope(Id),
  DuplicateFieldName(Str),
  DuplicateNamedArg(Id),
  DuplicateBinding(Id, usize, def::ExprDefKindMulti),
  Unused(Id, def::ExprDefKind),
  Incomparable(ty::Ty),
  MissingArgument(Id, ty::Ty),
  ExtraPositionalArgument(usize),
  ExtraNamedArgument(Id),
  InvalidPlus(ty::Ty, ty::Ty),
  CallNonFn(ty::Ty),
  InvalidLength(ty::Ty),
  Unify(Unify),
  InvalidSubscript(ty::Ty),
}

struct Display<'a> {
  multi_line: ty::display::MultiLine,
  kind: &'a Kind,
  store: &'a ty::GlobalStore,
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
      Kind::DuplicateBinding(id, _, _) => {
        write!(f, "duplicate binding: `{}`", id.display(self.str_ar))
      }
      Kind::Unused(id, _) => write!(f, "unused: `{}`", id.display(self.str_ar)),
      Kind::Incomparable(ty) => {
        let ty = ty.display(self.multi_line, self.store, None, self.str_ar);
        write!(f, "not a comparable type: `{ty}`")
      }
      Kind::MissingArgument(id, ty) => {
        let id = id.display(self.str_ar);
        let ty = ty.display(self.multi_line, self.store, None, self.str_ar);
        write!(f, "missing argument: `{id}` with type: `{ty}`")
      }
      Kind::ExtraPositionalArgument(n) => write!(f, "extra positional argument: {n}"),
      Kind::ExtraNamedArgument(id) => {
        let id = id.display(self.str_ar);
        write!(f, "extra named argument: `{id}`")
      }
      Kind::InvalidPlus(lhs, rhs) => {
        let lhs = lhs.display(self.multi_line, self.store, None, self.str_ar);
        let rhs = rhs.display(self.multi_line, self.store, None, self.str_ar);
        f.write_str("invalid `+`\n")?;
        writeln!(f, "  left:  `{lhs}`")?;
        write!(f, "  right: `{rhs}`")
      }
      Kind::CallNonFn(got) => {
        let got = got.display(self.multi_line, self.store, None, self.str_ar);
        write!(f, "expected a function type, found `{got}`")
      }
      Kind::InvalidLength(ty) => {
        let ty = ty.display(self.multi_line, self.store, None, self.str_ar);
        write!(f, "not a type which has length: `{ty}`")
      }
      Kind::Unify(u) => match u {
        Unify::Incompatible(want, got) => {
          let want = want.display(self.multi_line, self.store, None, self.str_ar);
          let got = got.display(self.multi_line, self.store, None, self.str_ar);
          f.write_str("incompatible types\n")?;
          writeln!(f, "  expected `{want}`")?;
          write!(f, "     found `{got}`")
        }
        Unify::MissingField(s) => write!(f, "missing field: `{}`", self.str_ar.get(s)),
        Unify::NotEnoughParams(want, got) => {
          f.write_str("not enough parameters\n")?;
          writeln!(f, "  expected at least {want}")?;
          write!(f, "   found only up to {got}")
        }
        Unify::MismatchedParamNames(want, got) => {
          f.write_str("mismatched parameter names\n")?;
          let want = want.display(self.str_ar);
          let got = got.display(self.str_ar);
          writeln!(f, "  expected `{want}`")?;
          write!(f, "     found `{got}`")
        }
        Unify::WantOptionalParamGotRequired(id) => {
          let id = id.display(self.str_ar);
          write!(f, "wanted an optional parameter, got a required one: `{id}`")
        }
        Unify::ExtraRequiredParam(id) => {
          let id = id.display(self.str_ar);
          write!(f, "extra required parameter: `{id}`")
        }
      },
      Kind::InvalidSubscript(ty) => {
        let ty = ty.display(self.multi_line, self.store, None, self.str_ar);
        write!(f, "not a type which can be subscripted with `[]` or `.`: `{ty}`")
      }
    }
  }
}
