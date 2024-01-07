use crate::{Expr, ExprMust, Id, StrArena};
use std::fmt;

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

/// # Errors
///
/// If getting the args failed.
pub fn get_a_b(positional: &[Expr], named: &[(Id, Expr)], expr: ExprMust) -> Result<[Expr; 2]> {
  if let Some(tma) = TooMany::new(2, positional.len(), named.len()) {
    return Err(Error { expr, kind: ErrorKind::TooMany(tma) });
  }
  let mut positional = positional.iter().copied();
  let mut a = positional.next();
  let mut b = positional.next();
  for &(arg_name, arg) in named {
    if arg_name == Id::a {
      match a {
        None => a = Some(arg),
        Some(_) => {
          return Err(Error { expr: arg.unwrap_or(expr), kind: ErrorKind::Duplicate(arg_name) })
        }
      }
    } else if arg_name == Id::b {
      match b {
        None => b = Some(arg),
        Some(_) => {
          return Err(Error { expr: arg.unwrap_or(expr), kind: ErrorKind::Duplicate(arg_name) })
        }
      }
    } else {
      return Err(Error { expr: arg.unwrap_or(expr), kind: ErrorKind::NotRequested(arg_name) });
    }
  }
  let Some(a) = a else {
    return Err(Error { expr, kind: ErrorKind::NotDefined(Id::a) });
  };
  let Some(b) = b else {
    return Err(Error { expr, kind: ErrorKind::NotDefined(Id::b) });
  };
  Ok([a, b])
}
