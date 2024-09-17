//! A thin wrapper around [`jsonnet_escape`].

use always::always;
use jsonnet_syntax::{ast, kind::SyntaxToken};

/// Get the escaped string from the ast string.
#[must_use]
pub fn get(string: &ast::String) -> String {
  match string.kind {
    ast::StringKind::DoubleQuotedString => slash(&string.token, b'"'),
    ast::StringKind::SingleQuotedString => slash(&string.token, b'\''),
    ast::StringKind::DoubleQuotedVerbatimString => verbatim(&string.token, b'"'),
    ast::StringKind::SingleQuotedVerbatimString => verbatim(&string.token, b'\''),
    ast::StringKind::TextBlock => {
      let mut sp_st = str_process::St::new(string.token.text());
      let mut out = EscapeOutput::new(string.token.text());
      always!(sp_st.eat_prefix(b"|||"));
      jsonnet_escape::text_block(&mut sp_st, &mut out);
      match String::from_utf8(out.bytes) {
        Ok(x) => x,
        Err(e) => {
          always!(false, "invalid utf-8: {e}");
          String::new()
        }
      }
    }
  }
}

fn slash(token: &SyntaxToken, delim: u8) -> String {
  let mut sp_st = str_process::St::new(token.text());
  let mut out = EscapeOutput::new(token.text());
  if sp_st.cur().is_some_and(|x| x == delim) {
    sp_st.bump();
  } else {
    let delim = delim.escape_ascii();
    always!(false, "no {delim} present");
    return String::new();
  }
  jsonnet_escape::slash(&mut sp_st, &mut out, delim);
  match String::from_utf8(out.bytes) {
    Ok(x) => x,
    Err(e) => {
      always!(false, "invalid utf-8 {e}");
      String::new()
    }
  }
}

fn verbatim(token: &SyntaxToken, delim: u8) -> String {
  let mut sp_st = str_process::St::new(token.text());
  let mut out = EscapeOutput::new(token.text());
  if sp_st.cur().is_some_and(|x| x == b'@') {
    sp_st.bump();
  } else {
    always!(false, "no @ present");
    return String::new();
  }
  if sp_st.cur().is_some_and(|x| x == delim) {
    sp_st.bump();
  } else {
    let delim = delim.escape_ascii();
    always!(false, "no {delim} present");
    return String::new();
  }
  jsonnet_escape::verbatim(&mut sp_st, &mut out, delim);
  match String::from_utf8(out.bytes) {
    Ok(x) => x,
    Err(e) => {
      always!(false, "invalid utf-8: {e}");
      String::new()
    }
  }
}

struct EscapeOutput {
  bytes: Vec<u8>,
}

impl EscapeOutput {
  fn new(text: &str) -> EscapeOutput {
    // usually has at least 2 delimiter bytes (1 at start, 1 at end), but may not if the token was
    // malformed (lex/parse error)
    let cap = text.len().saturating_sub(2);
    EscapeOutput { bytes: Vec::with_capacity(cap) }
  }
}

impl jsonnet_escape::Output for EscapeOutput {
  fn err(&mut self, _: usize, _: jsonnet_escape::Error) {
    // should have already been emitted as an error when lexing
  }

  fn byte(&mut self, b: u8) {
    self.bytes.push(b);
  }
}
