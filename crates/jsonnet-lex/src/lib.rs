//! Lexing a string into a sequence of tokens.

#![deny(clippy::pedantic, missing_debug_implementations, rust_2018_idioms)]

mod st;

use jsonnet_syntax::kind::SyntaxKind as SK;
use st::St;

#[derive(Debug, Default)]
pub struct Lex<'a> {
  pub tokens: Vec<token::Token<'a, SK>>,
  pub errors: Vec<&'static str>,
}

/// # Panics
///
/// Upon internal error.
#[must_use]
pub fn get(s: &str) -> Lex<'_> {
  let mut ret = Lex::default();
  let mut st = St::new(s);
  while let Some(b) = st.cur() {
    let start = st.mark();
    let kind = go(&mut st, b);
    let text = std::str::from_utf8(st.since_mark(start)).expect("each token should be a str");
    ret.tokens.push(token::Token { kind, text });
  }
  ret.errors = st.finish();
  ret
}

fn is_ws(b: u8) -> bool {
  matches!(b, b' ' | b'\t' | b'\n' | b'\r')
}

fn go(st: &mut St<'_>, b: u8) -> SK {
  if is_ws(b) {
    st.bump();
    st.advance_while(is_ws);
    return SK::Whitespace;
  }
  if b == b'#' {
    st.bump();
    st.advance_while(|b| b != b'\n');
    return SK::HashComment;
  }
  if b == b'/' {
    st.bump();
    match st.cur() {
      Some(b'/') => {
        st.bump();
        st.advance_while(|b| b != b'\n');
        return SK::SlashSlashComment;
      }
      Some(b'*') => {
        st.bump();
        let Some(mut prev) = st.cur() else {
          st.err("unclosed comment");
          return SK::BlockComment;
        };
        st.bump();
        while let Some(cur) = st.cur() {
          st.bump();
          if prev == b'*' && cur == b'/' {
            return SK::BlockComment;
          }
          prev = cur;
        }
        st.err("unclosed comment");
        return SK::BlockComment;
      }
      Some(_) | None => return SK::Slash,
    }
  }
  if let Some(&(_, sk)) = SK::PUNCTUATION.iter().find(|&(bs, _)| st.eat_prefix(bs)) {
    return sk;
  }
  if b.is_ascii_alphabetic() || b == b'_' {
    let start = st.mark();
    st.bump();
    st.advance_while(|b| b.is_ascii_alphanumeric() || b == b'_');
    // TODO reject `tailstrict`
    return SK::keyword(st.since_mark(start)).unwrap_or(SK::Id);
  }
  if b.is_ascii_digit() {
    st.bump();
    // TODO reject 0 followed by non-digit
    st.advance_while(|b| b.is_ascii_digit());
    if let Some(b'.') = st.cur() {
      st.bump();
      st.advance_while(|b| b.is_ascii_digit());
      // TODO reject if not advanced at all since bump (need at least one digit)
    }
    if let Some(b'e' | b'E') = st.cur() {
      st.bump();
      if let Some(b'-' | b'+') = st.cur() {
        st.bump();
      }
      st.advance_while(|b| b.is_ascii_digit());
      // TODO reject if not advanced at all since bump (need at least one digit)
    }
    return SK::Number;
  }
  if b == b'"' {
    st.bump();
    // TODO handle escapes
    while let Some(cur) = st.cur() {
      st.bump();
      if cur == b'"' {
        return SK::String;
      }
    }
    st.err("unclosed string");
    return SK::String;
  }
  // TODO handle more strings
  st.next_str();
  SK::Invalid
}
