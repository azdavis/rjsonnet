//! Static errors.

#![allow(clippy::too_many_lines)]

use crate::suggestion;
use jsonnet_expr::{def, ExprMust, Id, Str};
use jsonnet_ty::{self as ty, display::MultiLine};
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
    multi_line: MultiLine,
    store: &'a ty::GlobalStore,
    str_ar: &'a jsonnet_expr::StrArena,
  ) -> impl fmt::Display + 'a {
    Display { multi_line, kind: &self.kind, store, str_ar }
  }

  /// Returns the expr this error is for.
  #[must_use]
  pub fn expr_and_def(&self) -> (ExprMust, Option<def::ExprDefKind>) {
    let with_expr = match self.kind {
      Kind::UnusedVar(_, k) => Some(k),
      Kind::DuplicateVar(_, n, m) => Some(def::ExprDefKind::Multi(n, m)),
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
      Kind::UndefinedVar(..)
      | Kind::DuplicateField(_)
      | Kind::DuplicateNamedArg(_)
      | Kind::DuplicateVar(_, _, _) => diagnostic::Severity::Error,
      // it may be possible to eval the jsonnet without handling these, so we consider them
      // warnings. e.g. if the call with the missing/extra argument etc doesn't get eval'd.
      Kind::UnusedVar(_, _)
      | Kind::Unify(_)
      | Kind::MissingArg(_, _)
      | Kind::ExtraPositionalArg(_)
      | Kind::ExtraNamedArg(_)
      | Kind::Invalid(_, _)
      | Kind::AddSets
      | Kind::Unreachable => diagnostic::Severity::Warning,
    }
  }

  /// Apply a substitution.
  ///
  /// NOTE: no need to apply an expr subst because we run statics after combining the per-file
  /// syntax artifacts.
  pub fn apply(&mut self, ty_subst: &ty::Subst) {
    match &mut self.kind {
      Kind::MissingArg(_, ty)
      | Kind::Invalid(ty, Invalid::Call | Invalid::Length | Invalid::OrdCmp | Invalid::Subscript) =>
      {
        ty.apply(ty_subst);
      }
      Kind::Unify(Unify::Incompatible(a, b)) | Kind::Invalid(a, Invalid::Add(b)) => {
        a.apply(ty_subst);
        b.apply(ty_subst);
      }
      Kind::UndefinedVar(_, _)
      | Kind::DuplicateNamedArg(_)
      | Kind::DuplicateVar(_, _, _)
      | Kind::UnusedVar(_, _)
      | Kind::ExtraNamedArg(_)
      | Kind::DuplicateField(_)
      | Kind::ExtraPositionalArg(_)
      | Kind::Unify(
        Unify::WantOptionalParamGotRequired(_)
        | Unify::ExtraRequiredParam(_)
        | Unify::NoSuchField(_, _)
        | Unify::MismatchedParamNames(_, _)
        | Unify::NotEnoughParams(_, _),
      )
      | Kind::AddSets
      | Kind::Unreachable => {}
    }
  }
}

/// Errors specific to unification.
#[derive(Debug)]
pub(crate) enum Unify {
  Incompatible(ty::Ty, ty::Ty),
  NoSuchField(Str, Option<String>),
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
    let suggest = suggestion::approx(str_ar.get(no_such), obj.known.keys().map(|x| str_ar.get(x)));
    Some(Self::NoSuchField(no_such.clone(), suggest))
  }
}

#[derive(Debug)]
pub(crate) enum Kind {
  UndefinedVar(Id, Option<String>),
  DuplicateField(Str),
  DuplicateNamedArg(Id),
  DuplicateVar(Id, usize, def::ExprDefKindMulti),
  UnusedVar(Id, def::ExprDefKind),
  MissingArg(Id, ty::Ty),
  ExtraPositionalArg(usize),
  ExtraNamedArg(Id),
  Unify(Unify),
  Invalid(ty::Ty, Invalid),
  AddSets,
  Unreachable,
}

#[derive(Debug)]
pub(crate) enum Invalid {
  OrdCmp,
  Call,
  Subscript,
  Length,
  Add(ty::Ty),
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
      Kind::UndefinedVar(id, suggest) => {
        write!(f, "undefined variable: `{}`", id.display(self.str_ar))?;
        if let Some(suggest) = suggest {
          match self.multi_line {
            MultiLine::MustNot => f.write_str("; ")?,
            MultiLine::May => f.write_str("\n      ")?,
          }
          write!(f, "did you mean: `{suggest}`?")?;
        }
        Ok(())
      }
      Kind::DuplicateField(s) => write!(f, "duplicate field: `{}`", self.str_ar.get(s)),
      Kind::DuplicateNamedArg(id) => {
        write!(f, "duplicate named argument: `{}`", id.display(self.str_ar))
      }
      Kind::DuplicateVar(id, _, _) => {
        write!(f, "duplicate variable: `{}`", id.display(self.str_ar))
      }
      Kind::UnusedVar(id, _) => write!(f, "unused variable: `{}`", id.display(self.str_ar)),
      Kind::MissingArg(id, ty) => {
        let id = id.display(self.str_ar);
        let ty = ty.display(self.multi_line, self.store, None, self.str_ar);
        write!(f, "missing argument: `{id}`")?;
        match self.multi_line {
          MultiLine::MustNot => f.write_str(" ")?,
          MultiLine::May => f.write_str("\n       ")?,
        }
        write!(f, "with type: `{ty}`")
      }
      Kind::ExtraPositionalArg(n) => write!(f, "extra positional argument: {n}"),
      Kind::ExtraNamedArg(id) => {
        let id = id.display(self.str_ar);
        write!(f, "extra named argument: `{id}`")
      }
      Kind::Unify(u) => match u {
        Unify::Incompatible(want, got) => {
          let want = want.display(self.multi_line, self.store, None, self.str_ar);
          let got = got.display(self.multi_line, self.store, None, self.str_ar);
          f.write_str("incompatible types")?;
          match self.multi_line {
            MultiLine::MustNot => f.write_str("; ")?,
            MultiLine::May => f.write_str("\n  ")?,
          }
          write!(f, "expected `{want}`")?;
          match self.multi_line {
            MultiLine::MustNot => f.write_str("; ")?,
            MultiLine::May => f.write_str("\n     ")?,
          }
          write!(f, "found `{got}`")
        }
        Unify::NoSuchField(no_such, suggest) => {
          write!(f, "no such field: `{}`", self.str_ar.get(no_such))?;
          if let Some(suggest) = suggest {
            match self.multi_line {
              MultiLine::MustNot => f.write_str("; ")?,
              MultiLine::May => f.write_str("\n ")?,
            }
            write!(f, "did you mean: `{suggest}`?")?;
          }
          Ok(())
        }
        Unify::NotEnoughParams(want, got) => {
          f.write_str("not enough parameters")?;
          match self.multi_line {
            MultiLine::MustNot => f.write_str("; ")?,
            MultiLine::May => f.write_str("\n  ")?,
          }
          write!(f, "expected at least {want}")?;
          match self.multi_line {
            MultiLine::MustNot => f.write_str("; ")?,
            MultiLine::May => f.write_str("\n      ")?,
          }
          write!(f, "found up to {got}")
        }
        Unify::MismatchedParamNames(want, got) => {
          f.write_str("mismatched parameter names\n")?;
          match self.multi_line {
            MultiLine::MustNot => f.write_str("; ")?,
            MultiLine::May => f.write_str("\n  ")?,
          }
          let want = want.display(self.str_ar);
          write!(f, "  expected `{want}`")?;
          match self.multi_line {
            MultiLine::MustNot => f.write_str("; ")?,
            MultiLine::May => f.write_str("\n     ")?,
          }
          let got = got.display(self.str_ar);
          write!(f, "found `{got}`")
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
        Invalid::Add(rhs) => {
          let ty = ty.display(self.multi_line, self.store, None, self.str_ar);
          let rhs = rhs.display(self.multi_line, self.store, None, self.str_ar);
          f.write_str("not a pair of types that can be added with `+`")?;
          match self.multi_line {
            MultiLine::MustNot => f.write_str("; ")?,
            MultiLine::May => f.write_str("\n   ")?,
          }
          write!(f, "left: `{ty}`")?;
          match self.multi_line {
            MultiLine::MustNot => f.write_str("; ")?,
            MultiLine::May => f.write_str("\n  ")?,
          }
          write!(f, "right: `{rhs}`")
        }
      },
      Kind::AddSets => f.write_str("adding two sets will result in a non-set"),
      Kind::Unreachable => f.write_str("unreachable code"),
    }
  }
}

// impl fmt::Display for MultiLine {
//   fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//     match self {
//       MultiLine::MustNot => f.write_str("; "),
//       MultiLine::May => f.write_str("\n"),
//     }
//   }
// }
