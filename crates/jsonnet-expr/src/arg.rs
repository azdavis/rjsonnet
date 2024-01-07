use crate::{ExprMust, Id, StrArena};
use std::fmt;

pub use crate::generated::std_fn_args as std_fn;

#[derive(Debug)]
pub struct TooMany {
  params: usize,
  positional: usize,
  named: usize,
}

impl TooMany {
  #[must_use]
  pub fn new(params: usize, positional: usize, named: usize) -> Option<Self> {
    (positional + named > params).then_some(Self { params, positional, named })
  }
}

#[derive(Debug)]
pub struct Error {
  pub expr: ExprMust,
  pub kind: ErrorKind,
}

#[derive(Debug)]
pub enum ErrorKind {
  TooMany(TooMany),
  Duplicate(Id),
  NotRequested(Id),
  NotDefined(Id),
}

impl ErrorKind {
  /// Returns a value that displays this error.
  #[must_use]
  pub fn display<'a>(&'a self, ar: &'a StrArena) -> impl fmt::Display + 'a {
    DisplayError { kind: self, ar }
  }
}

struct DisplayError<'a> {
  kind: &'a ErrorKind,
  ar: &'a StrArena,
}

impl fmt::Display for DisplayError<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self.kind {
      ErrorKind::TooMany(tma) => write!(
        f,
        "too many arguments: have {} parameters, but got {} positional and {} named arguments",
        tma.params, tma.positional, tma.named
      ),
      ErrorKind::Duplicate(x) => write!(f, "duplicate argument: {}", x.display(self.ar)),
      ErrorKind::NotRequested(arg) => {
        write!(
          f,
          "argument `{}` was not requested at the function definition site",
          arg.display(self.ar)
        )
      }
      ErrorKind::NotDefined(arg) => {
        write!(f, "parameter `{}` was not defined at the function call site", arg.display(self.ar))
      }
    }
  }
}

pub type Result<T, E = Error> = std::result::Result<T, E>;
