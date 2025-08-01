//! Static errors.

#![expect(clippy::too_many_lines)]

use crate::suggestion;
use jsonnet_expr::{ExprMust, Id, Str, def};
use jsonnet_ty::{self as ty, display::Style};
use std::fmt;

const NONE: Option<std::convert::Infallible> = None;

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
    style: Style,
    store: &'a ty::GlobalStore,
    str_ar: &'a jsonnet_expr::StrArena,
  ) -> impl fmt::Display {
    Display { kind: &self.kind, cx: DisplayCx { style, store, str_ar } }
  }

  /// Returns the expr this error is for.
  #[must_use]
  pub fn expr_and_def(&self) -> (ExprMust, Option<def::ExprDefKind>) {
    let with_expr = match self.kind {
      Kind::UnusedVar { def, .. } => Some(def),
      Kind::DuplicateVar(_, n, m) => Some(def::ExprDefKind::Multi(n, m)),
      _ => None,
    };
    (self.expr, with_expr)
  }

  /// Returns the unused local error in this, if any.
  #[must_use]
  pub fn into_unused_local(self) -> Option<(ExprMust, Id, usize)> {
    if let Kind::UnusedVar { id, def, .. } = self.kind
      && let def::ExprDefKind::Multi(n, m) = def
      && let def::ExprDefKindMulti::LocalBind = m
    {
      Some((self.expr, id, n))
    } else {
      None
    }
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
      Kind::UnusedVar { .. }
      | Kind::Unify(_)
      | Kind::MissingArg(_, _)
      | Kind::ExtraPositionalArg(_)
      | Kind::ExtraNamedArg(_)
      | Kind::Invalid(_, _)
      | Kind::AddSets
      | Kind::Unreachable
      | Kind::NoSuchTupleIdx(_, _)
      | Kind::FormatParseFail(_)
      | Kind::FormatWrongCount(_, _)
      | Kind::FormatMissingMappingKey
      | Kind::FormatNoSuchField(_, _) => diagnostic::Severity::Warning,
    }
  }

  /// Apply a substitution.
  ///
  /// NOTE: no need to apply an expr subst because we run statics after combining the per-file
  /// syntax artifacts.
  pub fn apply(&mut self, subst: &ty::Subst) {
    match &mut self.kind {
      Kind::MissingArg(_, ty)
      | Kind::Invalid(ty, Invalid::Call | Invalid::Length | Invalid::Subscript) => {
        ty.apply(subst);
      }
      Kind::Unify(u) => u.apply(subst),
      Kind::Invalid(a, Invalid::Add(b) | Invalid::Eq(b) | Invalid::OrdCmp(b)) => {
        a.apply(subst);
        b.apply(subst);
      }
      Kind::UndefinedVar(_, _)
      | Kind::DuplicateNamedArg(_)
      | Kind::DuplicateVar(_, _, _)
      | Kind::UnusedVar { .. }
      | Kind::ExtraNamedArg(_)
      | Kind::DuplicateField(_)
      | Kind::ExtraPositionalArg(_)
      | Kind::AddSets
      | Kind::Unreachable
      | Kind::NoSuchTupleIdx(_, _)
      | Kind::FormatParseFail(_)
      | Kind::FormatWrongCount(_, _)
      | Kind::FormatMissingMappingKey
      | Kind::FormatNoSuchField(_, _) => {}
    }
  }
}

/// Errors specific to unification.
#[derive(Debug)]
pub(crate) enum Unify {
  Incompatible(ty::Ty, ty::Ty),
  NoSuchField(Str, Option<String>),
  WrongNumTupleElem(usize, usize),
  NotEnoughParams(usize, usize),
  MismatchedParamNames(Id, Id),
  WantOptionalParamGotRequired(Id),
  ExtraRequiredParam(Id),
}

impl Unify {
  pub(crate) fn no_such_field(
    str_ar: &jsonnet_expr::StrArena,
    obj: &jsonnet_ty::Object,
    no_such: Str,
  ) -> Option<Self> {
    if obj.has_unknown {
      return None;
    }
    let suggest = suggestion::approx(str_ar.get(no_such), obj.known.keys().map(|&x| str_ar.get(x)));
    Some(Self::NoSuchField(no_such, suggest))
  }

  fn apply(&mut self, subst: &ty::Subst) {
    match self {
      Unify::Incompatible(a, b) => {
        a.apply(subst);
        b.apply(subst);
      }
      Unify::WantOptionalParamGotRequired(_)
      | Unify::ExtraRequiredParam(_)
      | Unify::NoSuchField(_, _)
      | Unify::MismatchedParamNames(_, _)
      | Unify::NotEnoughParams(_, _)
      | Unify::WrongNumTupleElem(_, _) => {}
    }
  }
}

#[derive(Debug)]
pub(crate) enum Kind {
  UndefinedVar(Id, Option<String>),
  DuplicateField(Str),
  DuplicateNamedArg(Id),
  DuplicateVar(Id, usize, def::ExprDefKindMulti),
  UnusedVar { id: Id, def: def::ExprDefKind, can_silence: bool },
  MissingArg(Id, ty::Ty),
  ExtraPositionalArg(usize),
  ExtraNamedArg(Id),
  Unify(Unify),
  Invalid(ty::Ty, Invalid),
  AddSets,
  Unreachable,
  NoSuchTupleIdx(usize, usize),
  FormatParseFail(jsonnet_format_parse::ParseError),
  FormatWrongCount(usize, usize),
  FormatMissingMappingKey,
  FormatNoSuchField(String, Option<String>),
}

#[derive(Debug)]
pub(crate) enum Invalid {
  OrdCmp(ty::Ty),
  Eq(ty::Ty),
  Call,
  Subscript,
  Length,
  Add(ty::Ty),
}

#[derive(Debug, Clone, Copy)]
struct DisplayCx<'a> {
  style: Style,
  store: &'a ty::GlobalStore,
  str_ar: &'a jsonnet_expr::StrArena,
}

struct Display<'a> {
  kind: &'a Kind,
  cx: DisplayCx<'a>,
}

impl fmt::Display for Display<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match &self.kind {
      Kind::UndefinedVar(id, suggest) => {
        write!(f, "undefined variable: `{}`", id.display(self.cx.str_ar))?;
        if let Some(suggest) = suggest {
          match self.cx.style {
            Style::Short => f.write_str("; ")?,
            Style::Long => f.write_str("\n      ")?,
          }
          write!(f, "did you mean: `{suggest}`?")?;
        }
        Ok(())
      }
      Kind::DuplicateField(s) => write!(f, "duplicate field: `{}`", self.cx.str_ar.get(*s)),
      Kind::DuplicateNamedArg(id) => {
        write!(f, "duplicate named argument: `{}`", id.display(self.cx.str_ar))
      }
      Kind::DuplicateVar(id, _, _) => {
        write!(f, "duplicate variable: `{}`", id.display(self.cx.str_ar))
      }
      Kind::UnusedVar { id, can_silence, .. } => {
        write!(f, "unused variable: `{}`", id.display(self.cx.str_ar))?;
        match (self.cx.style, can_silence) {
          (Style::Short, _) | (_, false) => Ok(()),
          (Style::Long, true) => {
            f.write_str("\n  note: prefix the var with `_` to silence this warning")
          }
        }
      }
      Kind::MissingArg(id, ty) => {
        let id = id.display(self.cx.str_ar);
        let ty = ty.display(Style::Short, self.cx.store, None, self.cx.str_ar);
        write!(f, "missing argument: `{id}`")?;
        match self.cx.style {
          Style::Short => f.write_str(" ")?,
          Style::Long => f.write_str("\n       ")?,
        }
        write!(f, "with type: `{ty}`")
      }
      Kind::ExtraPositionalArg(n) => write!(f, "extra positional argument: {n}"),
      Kind::ExtraNamedArg(id) => {
        let id = id.display(self.cx.str_ar);
        write!(f, "extra named argument: `{id}`")
      }
      Kind::Unify(unify) => {
        let d = UnifyDisplay { unify, cx: self.cx };
        d.fmt(f)
      }
      Kind::Invalid(ty, inv) => match inv {
        Invalid::OrdCmp(rhs) => {
          f.write_str("invalid comparison")?;
          match self.cx.style {
            Style::Short => {}
            Style::Long => f.write_str(", i.e. use of `<`, `>=`, etc")?,
          }
          let elr = ExpectedLeftRight {
            expected: "comparable types",
            extra: "i.e. both `number`, both `string`, etc",
            left: Backticks(ty.display(Style::Short, self.cx.store, None, self.cx.str_ar)),
            right: Backticks(rhs.display(Style::Short, self.cx.store, None, self.cx.str_ar)),
            style: self.cx.style,
          };
          elr.fmt(f)
        }
        Invalid::Eq(rhs) => {
          f.write_str("invalid use of `==`")?;
          let elr = ExpectedLeftRight {
            expected: "equatable types",
            extra: "i.e. anything exception `function`",
            left: Backticks(ty.display(Style::Short, self.cx.store, None, self.cx.str_ar)),
            right: Backticks(rhs.display(Style::Short, self.cx.store, None, self.cx.str_ar)),
            style: self.cx.style,
          };
          elr.fmt(f)
        }
        Invalid::Call => {
          f.write_str("invalid call")?;
          let ef = ExpectedFound {
            expected: "a callable type",
            extra: Some("e.g. `function`"),
            found: Backticks(ty.display(Style::Short, self.cx.store, None, self.cx.str_ar)),
            style: self.cx.style,
          };
          ef.fmt(f)
        }
        Invalid::Subscript => {
          f.write_str("invalid subscript")?;
          match self.cx.style {
            Style::Short => {}
            Style::Long => f.write_str(", i.e. use of `[]` or `.`")?,
          }
          let ef = ExpectedFound {
            expected: "a type with fields or elements",
            extra: Some("i.e. `array[any]`, `object`, `string`"),
            found: Backticks(ty.display(Style::Short, self.cx.store, None, self.cx.str_ar)),
            style: self.cx.style,
          };
          ef.fmt(f)
        }
        Invalid::Length => {
          f.write_str("invalid call to `std.length`")?;
          let ef = ExpectedFound {
            expected: "a type with length",
            extra: Some("e.g. `array[any]`, `object`, `string`, `function`"),
            found: Backticks(ty.display(Style::Short, self.cx.store, None, self.cx.str_ar)),
            style: self.cx.style,
          };
          ef.fmt(f)
        }
        Invalid::Add(rhs) => {
          f.write_str("invalid use of `+`")?;
          let elr = ExpectedLeftRight {
            expected: "addable types",
            extra: "e.g. `number`, `string`, `object`, `array[any]`",
            left: Backticks(ty.display(Style::Short, self.cx.store, None, self.cx.str_ar)),
            right: Backticks(rhs.display(Style::Short, self.cx.store, None, self.cx.str_ar)),
            style: self.cx.style,
          };
          elr.fmt(f)
        }
      },
      Kind::AddSets => f.write_str("adding two sets will result in a non-set"),
      Kind::Unreachable => f.write_str("unreachable code"),
      Kind::NoSuchTupleIdx(len, idx) => {
        f.write_str("no such tuple index")?;
        let ef = ExpectedFound {
          expected: IdxInRange(*len),
          extra: NONE,
          found: Backticks(idx),
          style: self.cx.style,
        };
        ef.fmt(f)
      }
      Kind::FormatParseFail(e) => {
        write!(f, "invalid format string: {e}")
      }
      Kind::FormatWrongCount(want, got) => {
        f.write_str("wrong number of format arguments")?;
        let ef = ExpectedFound { expected: *want, extra: NONE, found: *got, style: self.cx.style };
        ef.fmt(f)
      }
      Kind::FormatMissingMappingKey => f.write_str("format specifier missing object field name"),
      Kind::FormatNoSuchField(field, suggest) => {
        write!(f, "no such field: `{field}`")?;
        if let Some(suggest) = suggest {
          match self.cx.style {
            Style::Short => f.write_str("; ")?,
            Style::Long => f.write_str("\n ")?,
          }
          write!(f, "did you mean: `{suggest}`?")?;
        }
        Ok(())
      }
    }
  }
}

struct UnifyDisplay<'a> {
  unify: &'a Unify,
  cx: DisplayCx<'a>,
}

impl fmt::Display for UnifyDisplay<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self.unify {
      Unify::Incompatible(want, got) => {
        f.write_str("incompatible types")?;
        let ef = ExpectedFound {
          expected: Backticks(want.display(Style::Short, self.cx.store, None, self.cx.str_ar)),
          extra: NONE,
          found: Backticks(got.display(Style::Short, self.cx.store, None, self.cx.str_ar)),
          style: self.cx.style,
        };
        ef.fmt(f)
      }
      Unify::NoSuchField(no_such, suggest) => {
        write!(f, "no such field: `{}`", self.cx.str_ar.get(*no_such))?;
        if let Some(suggest) = suggest {
          match self.cx.style {
            Style::Short => f.write_str("; ")?,
            Style::Long => f.write_str("\n ")?,
          }
          write!(f, "did you mean: `{suggest}`?")?;
        }
        Ok(())
      }
      Unify::NotEnoughParams(want, got) => {
        f.write_str("not enough parameters")?;
        let ef = ExpectedFound {
          expected: AtLeast(*want),
          extra: NONE,
          found: UpTo(*got),
          style: self.cx.style,
        };
        ef.fmt(f)
      }
      Unify::WrongNumTupleElem(want, got) => {
        f.write_str("wrong number of tuple elements")?;
        let ef = ExpectedFound { expected: *want, extra: NONE, found: *got, style: self.cx.style };
        ef.fmt(f)
      }
      Unify::MismatchedParamNames(want, got) => {
        f.write_str("mismatched parameter names")?;
        let ef = ExpectedFound {
          expected: Backticks(want.display(self.cx.str_ar)),
          extra: NONE,
          found: Backticks(got.display(self.cx.str_ar)),
          style: self.cx.style,
        };
        ef.fmt(f)
      }
      Unify::WantOptionalParamGotRequired(id) => {
        let id = id.display(self.cx.str_ar);
        write!(f, "mismatched parameter optionality: `{id}`")?;
        let ef = ExpectedFound {
          expected: "an optional parameter",
          extra: NONE,
          found: "a required parameter",
          style: self.cx.style,
        };
        ef.fmt(f)
      }
      Unify::ExtraRequiredParam(id) => {
        let id = id.display(self.cx.str_ar);
        write!(f, "extra required parameter: `{id}`")
      }
    }
  }
}

struct Backticks<B>(B);

impl<B> fmt::Display for Backticks<B>
where
  B: fmt::Display,
{
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "`{}`", self.0)
  }
}

struct ExpectedFound<E1, E2, F> {
  expected: E1,
  extra: Option<E2>,
  found: F,
  style: Style,
}

impl<E1, E2, F> fmt::Display for ExpectedFound<E1, E2, F>
where
  E1: fmt::Display,
  E2: fmt::Display,
  F: fmt::Display,
{
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self.style {
      Style::Short => f.write_str("; ")?,
      Style::Long => f.write_str("\n  ")?,
    }
    write!(f, "expected {}", self.expected)?;
    match self.style {
      Style::Short => f.write_str("; ")?,
      Style::Long => {
        if let Some(el) = &self.extra {
          write!(f, ", {el}")?;
        }
        f.write_str("\n     ")?;
      }
    }
    write!(f, "found {}", self.found)
  }
}

struct ExpectedLeftRight<L, R> {
  expected: &'static str,
  extra: &'static str,
  left: L,
  right: R,
  style: Style,
}

impl<L, R> fmt::Display for ExpectedLeftRight<L, R>
where
  L: fmt::Display,
  R: fmt::Display,
{
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self.style {
      Style::Short => f.write_str("; ")?,
      Style::Long => f.write_str("\n  ")?,
    }
    write!(f, "expected {}", self.expected)?;
    match self.style {
      Style::Short => f.write_str("; ")?,
      Style::Long => write!(f, ", {}\n   ", self.extra)?,
    }
    write!(f, "left: {}", self.left)?;
    match self.style {
      Style::Short => f.write_str("; ")?,
      Style::Long => f.write_str("\n  ")?,
    }
    write!(f, "right: {}", self.right)
  }
}

struct AtLeast(usize);

impl fmt::Display for AtLeast {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "at least {}", self.0)
  }
}

struct UpTo(usize);

impl fmt::Display for UpTo {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "up to {}", self.0)
  }
}

struct IdxInRange(usize);

impl fmt::Display for IdxInRange {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "an index in the range 0..{}", self.0)
  }
}
