//! Lexing a string into a sequence of tokens.

mod error;
mod internal;

use always::always;
use jsonnet_syntax::kind::SyntaxKind as SK;

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
#[must_use]
pub fn get(s: &str) -> Lex<'_> {
  let mut ret = Lex::default();
  let mut st = str_process::St::new(s);
  let mut out = error::Output::default();
  while let Some(b) = st.cur() {
    let start = st.mark();
    let kind = internal::token(&mut st, &mut out, b);
    let bs = st.non_empty_since(start);
    let text = match std::str::from_utf8(bs) {
      Ok(x) => x,
      Err(e) => {
        always!(false, "token not a str: {e}");
        "<ERROR>"
      }
    };
    ret.tokens.push(token::Token { kind, text });
  }
  ret.errors = out.finish();
  ret
}
