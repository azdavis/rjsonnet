//! Static errors.

#![allow(clippy::too_many_lines)]

use crate::suggestion;
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
      // these static checks happen before eval, i.e. if they are present, we cannot eval. so they
      // are errors.
      Kind::UndefinedVar(_)
      | Kind::DuplicateFieldName(_)
      | Kind::DuplicateNamedArg(_)
      | Kind::DuplicateBinding(_, _, _) => diagnostic::Severity::Error,
      // it may be possible to eval the jsonnet  without handling these, so we consider them
      // warnings. e.g. if the call with the missing/extra argument etc doesn't get eval'd.
      Kind::Unused(_, _)
      | Kind::Unify(_)
      | Kind::MissingArgument(_, _)
      | Kind::ExtraPositionalArgument(_)
      | Kind::ExtraNamedArgument(_)
      | Kind::Invalid(_, _) => diagnostic::Severity::Warning,
    }
  }

  /// Apply a substitution.
  ///
  /// NOTE: no need to apply an expr subst because we run statics after combining the per-file
  /// syntax artifacts.
  pub fn apply(&mut self, ty_subst: &ty::Subst) {
    match &mut self.kind {
      Kind::MissingArgument(_, ty)
      | Kind::Invalid(ty, Invalid::Call | Invalid::Length | Invalid::OrdCmp | Invalid::Subscript) =>
      {
        ty.apply(ty_subst);
      }
      Kind::Unify(Unify::Incompatible(a, b)) | Kind::Invalid(a, Invalid::Plus(b)) => {
        a.apply(ty_subst);
        b.apply(ty_subst);
      }
      Kind::UndefinedVar(_)
      | Kind::DuplicateNamedArg(_)
      | Kind::DuplicateBinding(_, _, _)
      | Kind::Unused(_, _)
      | Kind::ExtraNamedArgument(_)
      | Kind::DuplicateFieldName(_)
      | Kind::ExtraPositionalArgument(_)
      | Kind::Unify(
        Unify::WantOptionalParamGotRequired(_)
        | Unify::ExtraRequiredParam(_)
        | Unify::NoSuchField(..)
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
  NoSuchField(Str, Option<Str>),
  NotEnoughParams(usize, usize),
  MismatchedParamNames(Id, Id),
  WantOptionalParamGotRequired(Id),
  ExtraRequiredParam(Id),
}

impl Unify {
  pub(crate) fn no_such_field(
    str_ar: &jsonnet_expr::StrArena,
    obj: &jsonnet_ty::Object,
    no_such: &Str,
  ) -> Option<Self> {
    if obj.has_unknown {
      return None;
    }
    let suggest = suggestion::approx(str_ar, str_ar.get(no_such), obj.known.keys());
    Some(Self::NoSuchField(no_such.clone(), suggest))
  }
}

#[derive(Debug)]
pub(crate) enum Kind {
  UndefinedVar(Id),
  DuplicateFieldName(Str),
  DuplicateNamedArg(Id),
  DuplicateBinding(Id, usize, def::ExprDefKindMulti),
  Unused(Id, def::ExprDefKind),
  MissingArgument(Id, ty::Ty),
  ExtraPositionalArgument(usize),
  ExtraNamedArgument(Id),
  Unify(Unify),
  Invalid(ty::Ty, Invalid),
}

#[derive(Debug)]
pub(crate) enum Invalid {
  OrdCmp,
  Call,
  Subscript,
  Length,
  Plus(ty::Ty),
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
      Kind::UndefinedVar(id) => {
        write!(f, "undefined variable: `{}`", id.display(self.str_ar))?;
        if let Some(suggest) = suggestion::exact(self.str_ar.get_id(*id)) {
          write!(f, "; did you mean `{suggest}`?")?;
        }
        Ok(())
      }
      Kind::DuplicateFieldName(s) => write!(f, "duplicate field name: `{}`", self.str_ar.get(s)),
      Kind::DuplicateNamedArg(id) => {
        write!(f, "duplicate named argument: `{}`", id.display(self.str_ar))
      }
      Kind::DuplicateBinding(id, _, _) => {
        write!(f, "duplicate binding: `{}`", id.display(self.str_ar))
      }
      Kind::Unused(id, _) => write!(f, "unused: `{}`", id.display(self.str_ar)),
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
      Kind::Unify(u) => match u {
        Unify::Incompatible(want, got) => {
          let want = want.display(self.multi_line, self.store, None, self.str_ar);
          let got = got.display(self.multi_line, self.store, None, self.str_ar);
          f.write_str("incompatible types\n")?;
          writeln!(f, "  expected `{want}`")?;
          write!(f, "     found `{got}`")
        }
        Unify::NoSuchField(no_such, suggest) => {
          write!(f, "no such field: `{}`", self.str_ar.get(no_such))?;
          if let Some(suggest) = suggest {
            write!(f, "; did you mean `{}`?", self.str_ar.get(suggest))?;
          }
          Ok(())
        }
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
      Kind::Invalid(ty, inv) => match inv {
        Invalid::OrdCmp => {
          let ty = ty.display(self.multi_line, self.store, None, self.str_ar);
          write!(f, "not a type that can be compared with <, >=, etc: `{ty}`")
        }
        Invalid::Call => {
          let ty = ty.display(self.multi_line, self.store, None, self.str_ar);
          write!(f, "not a type that can be called: `{ty}`")
        }
        Invalid::Subscript => {
          let ty = ty.display(self.multi_line, self.store, None, self.str_ar);
          write!(f, "not a type which can be subscripted with `[]` or `.`: `{ty}`")
        }
        Invalid::Length => {
          let ty = ty.display(self.multi_line, self.store, None, self.str_ar);
          write!(f, "not a type which has length: `{ty}`")
        }
        Invalid::Plus(rhs) => {
          let ty = ty.display(self.multi_line, self.store, None, self.str_ar);
          let rhs = rhs.display(self.multi_line, self.store, None, self.str_ar);
          f.write_str("not a pair of types that can be added with `+`\n")?;
          writeln!(f, "  left:  `{ty}`")?;
          write!(f, "  right: `{rhs}`")
        }
      },
    }
  }
}
