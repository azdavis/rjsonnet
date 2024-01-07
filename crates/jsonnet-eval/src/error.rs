use jsonnet_expr::arg;
use std::fmt::{self, Debug};

pub type Result<T, E = Error> = std::result::Result<T, E>;

#[derive(Debug)]
pub enum Error {
  Exec { expr: jsonnet_expr::ExprMust, kind: Kind },
  ManifestFn,
}

impl Error {
  #[must_use]
  pub fn display<'a>(&'a self, ar: &'a jsonnet_expr::StrArena) -> impl fmt::Display + 'a {
    DisplayError { error: self, ar }
  }
}

impl From<arg::Error> for Error {
  fn from(e: arg::Error) -> Self {
    Error::Exec { expr: e.expr, kind: e.kind.into() }
  }
}

#[derive(Debug)]
pub enum Kind {
  ArrayIdxNotInteger,
  ArrayIdxOutOfRange,
  DuplicateField(jsonnet_expr::Str),
  IncompatibleTypes,
  FieldNotDefined(jsonnet_expr::Str),
  Arg(arg::ErrorKind),
  Infinite(jsonnet_expr::Infinite),
  User(jsonnet_expr::Str),
}

impl From<arg::ErrorKind> for Kind {
  fn from(ek: arg::ErrorKind) -> Self {
    Self::Arg(ek)
  }
}

struct DisplayError<'a> {
  error: &'a Error,
  ar: &'a jsonnet_expr::StrArena,
}

impl fmt::Display for DisplayError<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self.error {
      Error::Exec { kind, .. } => match kind {
        Kind::ArrayIdxNotInteger => f.write_str("array index not an integer"),
        Kind::ArrayIdxOutOfRange => f.write_str("array index out of range"),
        Kind::DuplicateField(x) => write!(f, "duplicate field: {}", self.ar.get(x)),
        Kind::IncompatibleTypes => f.write_str("incompatible types"),
        Kind::FieldNotDefined(name) => {
          write!(f, "field `{}` not defined", self.ar.get(name))
        }
        Kind::Arg(ek) => ek.display(self.ar).fmt(f),
        Kind::Infinite(inf) => write!(f, "infinite number: {inf}"),
        Kind::User(s) => write!(f, "explicit `error`: {}", self.ar.get(s)),
      },
      Error::ManifestFn => f.write_str("cannot manifest function"),
    }
  }
}
