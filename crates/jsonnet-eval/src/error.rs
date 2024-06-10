//! Errors.

use jsonnet_expr::{arg, Id, Str};
use std::fmt::{self, Debug};

pub type Result<T, E = Error> = std::result::Result<T, E>;

#[derive(Debug, Clone)]
pub enum Error {
  Exec { expr: jsonnet_expr::ExprMust, kind: Kind },
  ManifestFn,
  NoPath(paths::PathId),
  NoExpr,
}

impl Error {
  #[must_use]
  pub fn display<'a>(
    &'a self,
    ar: &'a jsonnet_expr::StrArena,
    paths: &'a paths::Store,
    relative_to: Option<&'a paths::CleanPath>,
  ) -> impl fmt::Display + 'a {
    DisplayError { error: self, ar, paths, relative_to }
  }
}

impl From<arg::Error> for Error {
  fn from(e: arg::Error) -> Self {
    Error::Exec { expr: e.expr, kind: e.kind.into() }
  }
}

#[derive(Debug, Clone)]
pub struct Cycle {
  pub first_and_last: paths::PathId,
  pub intervening: Vec<paths::PathId>,
}

#[derive(Debug, Clone)]
pub enum Kind {
  Todo(&'static str),
  ArrayIdxNotInteger,
  ArrayIdxOutOfRange,
  DuplicateField(Str),
  IncompatibleTypes,
  FieldNotDefined(Str),
  Arg(arg::ErrorKind),
  Infinite(finite_float::Infinite),
  User(Str),
  /// should be caught in statics
  NotInScope(Id),
  Cycle(Cycle),
}

impl From<arg::ErrorKind> for Kind {
  fn from(ek: arg::ErrorKind) -> Self {
    Self::Arg(ek)
  }
}

struct DisplayError<'a> {
  error: &'a Error,
  ar: &'a jsonnet_expr::StrArena,
  paths: &'a paths::Store,
  relative_to: Option<&'a paths::CleanPath>,
}

impl<'a> DisplayError<'a> {
  fn display_path(&self, path_id: paths::PathId) -> impl fmt::Display + 'a {
    let mut p = self.paths.get_path(path_id).as_path();
    if let Some(r) = self.relative_to {
      p = p.strip_prefix(r.as_path()).unwrap_or(p);
    }
    p.display()
  }
}

impl fmt::Display for DisplayError<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self.error {
      Error::Exec { kind, .. } => match kind {
        Kind::Todo(s) => write!(f, "not yet implemented: {s}"),
        Kind::ArrayIdxNotInteger => f.write_str("array index not an integer"),
        Kind::ArrayIdxOutOfRange => f.write_str("array index out of range"),
        Kind::DuplicateField(x) => write!(f, "duplicate field: `{}`", self.ar.get(x)),
        Kind::IncompatibleTypes => f.write_str("incompatible types"),
        Kind::FieldNotDefined(name) => {
          write!(f, "field `{}` not defined", self.ar.get(name))
        }
        Kind::Arg(ek) => ek.display(self.ar).fmt(f),
        Kind::Infinite(inf) => write!(f, "infinite number: {inf}"),
        Kind::User(s) => write!(f, "explicit `error`: {}", self.ar.get(s)),
        Kind::NotInScope(id) => write!(f, "not in scope: `{}`", id.display(self.ar)),
        Kind::Cycle(cycle) => {
          let first_and_last = self.display_path(cycle.first_and_last);
          write!(f, "import cycle: {first_and_last} -> ")?;
          for &path in &cycle.intervening {
            let path = self.display_path(path);
            write!(f, "{path} -> ")?;
          }
          write!(f, "{first_and_last}")
        }
      },
      Error::ManifestFn => f.write_str("cannot manifest function"),
      Error::NoPath(p) => {
        let p = self.display_path(*p);
        write!(f, "no such path: {p}")
      }
      Error::NoExpr => write!(f, "no expr"),
    }
  }
}
