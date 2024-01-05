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
pub struct TooManyArguments {
  params: usize,
  positional: usize,
  named: usize,
}

impl TooManyArguments {
  pub(crate) fn new(params: usize, positional: usize, named: usize) -> Option<Self> {
    (positional + named > params).then_some(Self { params, positional, named })
  }
}

#[derive(Debug)]
pub enum Kind {
  ArrayIdxNotInteger,
  ArrayIdxOutOfRange,
  DuplicateArgument(jsonnet_expr::Id),
  DuplicateField(jsonnet_expr::Str),
  IncompatibleTypes,
  NoSuchArgument(jsonnet_expr::Id),
  NoSuchField(jsonnet_expr::Str),
  TooManyArguments(TooManyArguments),
  Infinite(jsonnet_expr::Infinite),
  User(jsonnet_expr::Str),
  /// TODO remove this and allow named args for std fns
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
        Kind::ArrayIdxNotInteger => f.write_str("array index not an integer"),
        Kind::ArrayIdxOutOfRange => f.write_str("array index out of range"),
        Kind::DuplicateArgument(x) => write!(f, "duplicate argument: {}", x.display(self.ar)),
        Kind::DuplicateField(x) => write!(f, "duplicate field: {}", self.ar.get(x)),
        Kind::IncompatibleTypes => f.write_str("incompatible types"),
        Kind::NoSuchArgument(arg) => write!(f, "no such argument: {}", arg.display(self.ar)),
        Kind::NoSuchField(name) => write!(f, "no such field: {}", self.ar.get(name)),
        Kind::TooManyArguments(tma) => write!(
          f,
          "too many arguments: have {} parameters, but got {} positional and {} named arguments",
          tma.params, tma.positional, tma.named
        ),
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
