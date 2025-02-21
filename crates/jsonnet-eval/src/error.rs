//! Errors.

use jsonnet_expr::{Id, Str, arg};
use std::fmt::{self, Debug};

pub type Result<T, E = Error> = std::result::Result<T, E>;

#[derive(Debug, Clone)]
pub enum Error {
  Exec { expr: jsonnet_expr::ExprMust, kind: Kind },
  ManifestFn,
  NoPath(paths::PathId),
  HasErrors(paths::PathId),
  NoExpr,
}

impl Error {
  #[must_use]
  pub fn display<'a>(
    &'a self,
    ar: &'a jsonnet_expr::StrArena,
    paths: &'a paths::Store,
    relative_to: Option<&'a paths::CleanPath>,
  ) -> impl fmt::Display {
    ErrorDisplay { error: self, ar, paths, relative_to }
  }
}

impl From<arg::Error> for Error {
  fn from(e: arg::Error) -> Self {
    Error::Exec { expr: e.expr, kind: e.kind.into() }
  }
}

#[derive(Debug, Clone)]
pub enum Kind {
  Todo(&'static str),
  DuplicateField(Str),
  IncompatibleTypes,
  NoSuchField(Str),
  Arg(arg::ErrorKind),
  Infinite(finite_float::Infinite),
  User(Str),
  /// should be caught in statics
  UndefinedVar(Id),
  Cycle(cycle::Cycle<paths::PathId>),
  IdxNotInteger(finite_float::Float),
  IdxOutOfRangeF(finite_float::Float),
  IdxOutOfRange(usize),
  IdxNotUtf8Boundary(usize),
  EqFn,
}

impl From<arg::ErrorKind> for Kind {
  fn from(ek: arg::ErrorKind) -> Self {
    Self::Arg(ek)
  }
}

struct ErrorDisplay<'a> {
  error: &'a Error,
  ar: &'a jsonnet_expr::StrArena,
  paths: &'a paths::Store,
  relative_to: Option<&'a paths::CleanPath>,
}

impl ErrorDisplay<'_> {
  fn display_path(&self, path_id: paths::PathId) -> impl fmt::Display {
    let mut p = self.paths.get_path(path_id).as_path();
    if let Some(r) = self.relative_to {
      p = p.strip_prefix(r.as_path()).unwrap_or(p);
    }
    p.display()
  }
}

impl fmt::Display for ErrorDisplay<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self.error {
      Error::Exec { kind, .. } => match kind {
        Kind::Todo(s) => write!(f, "not yet implemented: {s}"),
        Kind::DuplicateField(x) => write!(f, "duplicate field: `{}`", self.ar.get(*x)),
        Kind::IncompatibleTypes => f.write_str("incompatible types"),
        Kind::NoSuchField(name) => {
          write!(f, "no such field: `{}`", self.ar.get(*name))
        }
        Kind::Arg(ek) => ek.display(self.ar).fmt(f),
        Kind::Infinite(inf) => write!(f, "infinite number: {inf}"),
        Kind::User(s) => write!(f, "explicit `error`: {}", self.ar.get(*s)),
        Kind::UndefinedVar(id) => write!(f, "undefined variable: `{}`", id.display(self.ar)),
        Kind::Cycle(cycle) => {
          let first_and_last = self.display_path(cycle.first_and_last);
          write!(f, "import cycle: {first_and_last} -> ")?;
          for &path in &cycle.intervening {
            let path = self.display_path(path);
            write!(f, "{path} -> ")?;
          }
          write!(f, "{first_and_last}")
        }
        Kind::IdxNotInteger(n) => write!(f, "index not an integer: `{n}`"),
        Kind::IdxOutOfRangeF(n) => write!(f, "index out of range: {n}"),
        Kind::IdxOutOfRange(n) => write!(f, "index out of range: {n}"),
        Kind::IdxNotUtf8Boundary(n) => write!(f, "index not on UTF-8 boundary: {n}"),
        Kind::EqFn => f.write_str("cannot test equality of functions"),
      },
      Error::ManifestFn => f.write_str("cannot manifest function"),
      Error::NoPath(p) => {
        let p = self.display_path(*p);
        write!(f, "no such path: {p}")
      }
      Error::HasErrors(p) => {
        let p = self.display_path(*p);
        write!(f, "cannot manifest, has errors: {p}")
      }
      Error::NoExpr => write!(f, "no expr"),
    }
  }
}
