//! Parse a sequence of tokens into a concrete syntax tree.

mod internal;

use jsonnet_syntax::kind::SyntaxKind as SK;
use std::fmt;

/// Does the parsing.
#[must_use]
pub fn get(tokens: &[token::Token<'_, SK>]) -> Parse {
  let mut p = Parser::new(tokens);
  let en = p.enter();
  internal::expr_must(&mut p);
  // could have this be a while loop, but then we'd just get a lot of errors
  if p.peek().is_some() {
    p.error(ErrorKind::Trailing);
    p.bump();
  }
  p.exit(en, SK::Root);
  let mut sink = event_parse::rowan_sink::RowanSink::default();
  p.finish(&mut sink);
  let (node, errors) = sink.finish::<jsonnet_syntax::kind::Jsonnet>();
  Parse {
    root: jsonnet_syntax::Root::new(node.green().into_owned()),
    errors: errors.into_iter().map(Error).collect(),
  }
}

type Parser<'a> = event_parse::Parser<'a, SK, ErrorKind>;

/// The result of parsing.
#[derive(Debug)]
pub struct Parse {
  /// The parsed concrete syntax tree.
  pub root: jsonnet_syntax::Root,
  /// Errors when parsing.
  pub errors: Vec<Error>,
}

/// A parse error.
#[derive(Debug)]
pub struct Error(event_parse::rowan_sink::Error<ErrorKind>);

impl Error {
  /// The range of the error.
  #[must_use]
  pub fn range(&self) -> text_size::TextRange {
    self.0.range
  }
}

impl fmt::Display for Error {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match &self.0.kind {
      ErrorKind::Trailing => f.write_str("trailing token"),
      ErrorKind::Expected(e) => write!(f, "expected {e}"),
    }
  }
}

#[derive(Debug)]
enum ErrorKind {
  Trailing,
  Expected(Expected),
}

impl event_parse::Expected<SK> for ErrorKind {
  fn expected(kind: SK) -> Self {
    ErrorKind::Expected(Expected::Kind(kind))
  }
}

#[derive(Debug)]
enum Expected {
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
