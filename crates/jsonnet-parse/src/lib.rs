//! Parse a sequence of tokens into a concrete syntax tree.

#![deny(clippy::pedantic, missing_debug_implementations, rust_2018_idioms)]

mod internal;

use jsonnet_syntax::{ast::AstNode as _, kind::SyntaxKind as SK};
use std::fmt;

/// # Panics
///
/// Upon internal error.
#[must_use]
pub fn get(tokens: &[token::Token<'_, SK>]) -> Parse {
  let mut p = Parser::new(tokens);
  let en = p.enter();
  internal::expr_must(&mut p);
  while p.peek().is_some() {
    p.error(ErrorKind::Trailing);
    p.bump();
  }
  p.exit(en, SK::Root);
  let mut sink = event_parse::rowan_sink::RowanSink::default();
  p.finish(&mut sink);
  let (node, errors) = sink.finish::<jsonnet_syntax::kind::Jsonnet>();
  let root = jsonnet_syntax::ast::Root::cast(node).expect("root should be Root");
  Parse { root, errors: errors.into_iter().map(Error).collect() }
}

pub(crate) type Parser<'a> = event_parse::Parser<'a, SK, ErrorKind>;

#[derive(Debug)]
pub struct Parse {
  pub root: jsonnet_syntax::ast::Root,
  pub errors: Vec<Error>,
}

#[derive(Debug)]
pub struct Error(event_parse::rowan_sink::Error<ErrorKind>);

impl fmt::Display for Error {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match &self.0.kind {
      ErrorKind::Trailing => f.write_str("trailing token"),
      ErrorKind::Expected(e) => write!(f, "expected {e}"),
    }
  }
}

#[derive(Debug)]
pub enum ErrorKind {
  Trailing,
  Expected(Expected),
}

impl event_parse::Expected<SK> for ErrorKind {
  fn expected(kind: SK) -> Self {
    ErrorKind::Expected(Expected::Kind(kind))
  }
}

#[derive(Debug)]
pub enum Expected {
  Expr,
  Kind(SK),
  Visibility,
  String,
}

impl fmt::Display for Expected {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Expected::Expr => f.write_str("expression"),
      Expected::Kind(k) => k.fmt(f),
      Expected::Visibility => f.write_str("field visibility modifier"),
      Expected::String => f.write_str("string"),
    }
  }
}
