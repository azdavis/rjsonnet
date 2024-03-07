//! Utilities for function arguments.

use crate::{ExprMust, Id, StrArena};
use std::fmt;

#[derive(Debug, Clone)]
pub struct TooMany {
  params: Vec<Id>,
  positional: usize,
  named: Vec<Id>,
}

impl TooMany {
  #[must_use]
  pub fn new<Params, Named>(params: Params, positional: usize, named: Named) -> Option<Self>
  where
    Params: ExactSizeIterator<Item = Id>,
    Named: ExactSizeIterator<Item = Id>,
  {
    (positional + named.len() > params.len()).then(|| Self {
      params: params.collect(),
      positional,
      named: named.collect(),
    })
  }
}

#[derive(Debug)]
pub struct Error {
  pub expr: ExprMust,
  pub kind: ErrorKind,
}

#[derive(Debug, Clone)]
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
      ErrorKind::TooMany(tma) => {
        writeln!(f, "too many arguments")?;

        let params = DisplayIds { name: "parameters", ar: self.ar, ids: &tma.params };
        writeln!(f, "  {params}")?;

        write!(f, "  positional arguments: ",)?;
        if tma.positional == 0 {
          writeln!(f, "<none>")?;
        } else {
          writeln!(f, "{}", tma.positional)?;
        }

        let named = DisplayIds { name: "named arguments", ar: self.ar, ids: &tma.named };
        writeln!(f, "  {named}")?;
        Ok(())
      }
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

struct DisplayIds<'a> {
  name: &'static str,
  ar: &'a StrArena,
  ids: &'a [Id],
}

impl<'a> fmt::Display for DisplayIds<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let mut iter = self.ids.iter();
    let Some(fst) = iter.next() else {
      return write!(f, "{}: <none>", self.name);
    };
    write!(f, "{} ({}): ", self.name, self.ids.len())?;
    fst.display(self.ar).fmt(f)?;
    for x in iter {
      write!(f, ", {}", x.display(self.ar))?;
    }
    Ok(())
  }
}
