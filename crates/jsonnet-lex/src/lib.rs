//! Lexing a string into a sequence of tokens.

#![deny(clippy::pedantic, missing_debug_implementations, missing_docs, rust_2018_idioms)]

mod error;
mod internal;
mod st;

use jsonnet_syntax::kind::SyntaxKind as SK;
use st::St;

pub use error::Error;

/// The result of lexing.
#[derive(Debug, Default)]
pub struct Lex<'a> {
  /// The tokens.
  pub tokens: Vec<token::Token<'a, SK>>,
  /// Errors when lexing.
  pub errors: Vec<Error>,
}

/// Transforms a string into tokens.
///
/// # Panics
///
/// Upon internal error.
#[must_use]
pub fn get(s: &str) -> Lex<'_> {
  let mut ret = Lex::default();
  let mut st = St::new(s);
  while let Some(b) = st.inner.cur() {
    let start = st.inner.mark();
    let kind = internal::token(&mut st, b);
    let bs = st.inner.non_empty_since(start);
    let text = std::str::from_utf8(bs).expect("each token should be a str");
    ret.tokens.push(token::Token { kind, text });
  }
  ret.errors = st.finish();
  ret
}
