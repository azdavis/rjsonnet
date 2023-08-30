//! Parse a sequence of tokens into a concrete syntax tree.

#![deny(clippy::pedantic, missing_debug_implementations, rust_2018_idioms)]

mod expr;

use jsonnet_syntax::{ast::AstNode as _, kind::SyntaxKind as SK};

/// # Panics
///
/// Upon internal error.
#[must_use]
pub fn get(tokens: &[token::Token<'_, SK>]) -> Parse {
  let mut p = Parser::new(tokens);
  let en = p.enter();
  expr::expr_must(&mut p);
  while p.peek().is_some() {
    p.error(ErrorKind::Trailing);
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

#[derive(Debug)]
pub enum ErrorKind {
  Trailing,
  Expected(Expected),
}

#[derive(Debug)]
pub enum Expected {
  Expr,
  Kind(SK),
  Visibility,
}

impl event_parse::Expected<SK> for ErrorKind {
  fn expected(kind: SK) -> Self {
    ErrorKind::Expected(Expected::Kind(kind))
  }
}
