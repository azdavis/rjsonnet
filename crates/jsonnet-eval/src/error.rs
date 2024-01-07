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
pub struct TooManyArgs {
  params: usize,
  positional: usize,
  named: usize,
}

impl TooManyArgs {
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
  TooManyArgs(TooManyArgs),
  ArgNotRequested(jsonnet_expr::Id),
  ParamNotDefined(jsonnet_expr::Id),
  FieldNotDefined(jsonnet_expr::Str),
  Infinite(jsonnet_expr::Infinite),
  User(jsonnet_expr::Str),
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
        Kind::TooManyArgs(tma) => write!(
          f,
          "too many arguments: have {} parameters, but got {} positional and {} named arguments",
          tma.params, tma.positional, tma.named
        ),
        Kind::ArgNotRequested(arg) => {
          write!(
            f,
            "argument `{}` was not requested at the function definition site",
            arg.display(self.ar)
          )
        }
        Kind::ParamNotDefined(arg) => {
          write!(
            f,
            "parameter `{}` was not defined at the function call site",
            arg.display(self.ar)
          )
        }
        Kind::FieldNotDefined(name) => {
          write!(f, "field `{}` not defined", self.ar.get(name))
        }
        Kind::Infinite(inf) => write!(f, "infinite number: {inf}"),
        Kind::User(s) => write!(f, "explicit `error`: {}", self.ar.get(s)),
      },
      Error::ManifestFn => f.write_str("cannot manifest function"),
    }
  }
}
