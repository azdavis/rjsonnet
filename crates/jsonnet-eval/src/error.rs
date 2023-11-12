use std::fmt;

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

#[derive(Debug)]
pub enum Kind {
  Todo(&'static str),
  ArrayIdxNotInteger,
  ArrayIdxOutOfRange,
  DuplicateArgument,
  DuplicateField,
  IncompatibleTypes,
  NoSuchArgument,
  NoSuchFieldName,
  TooManyArguments,
  Infinite(jsonnet_expr::Infinite),
  User(jsonnet_expr::Str),
  StdFuncNamedArgs,
  StdFuncWrongNumArgs(usize, usize),
}

struct DisplayError<'a> {
  error: &'a Error,
  ar: &'a jsonnet_expr::StrArena,
}

impl fmt::Display for DisplayError<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self.error {
      Error::Exec { kind, .. } => match kind {
        Kind::Todo(s) => write!(f, "TODO: {s}"),
        Kind::ArrayIdxNotInteger => f.write_str("array index not an integer"),
        Kind::ArrayIdxOutOfRange => f.write_str("array index out of range"),
        Kind::DuplicateArgument => f.write_str("duplicate argument"),
        Kind::DuplicateField => f.write_str("duplicate field"),
        Kind::IncompatibleTypes => f.write_str("incompatible types"),
        Kind::NoSuchArgument => f.write_str("no such argument"),
        Kind::NoSuchFieldName => f.write_str("no such field"),
        Kind::TooManyArguments => f.write_str("too many arguments"),
        Kind::Infinite(inf) => write!(f, "infinite number: {inf}"),
        Kind::User(s) => write!(f, "explicit `error`: {}", self.ar.get(s)),
        Kind::StdFuncNamedArgs => f.write_str("named arguments to a `std` function"),
        Kind::StdFuncWrongNumArgs(want, got) => {
          write!(f, "expected {want} but found {got} arguments to a `std` function")
        }
      },
      Error::ManifestFn => f.write_str("cannot manifest function"),
    }
  }
}