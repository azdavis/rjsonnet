//! A thin wrapper around [`jsonnet_escape`].

use always::always;
use jsonnet_syntax::ast;

/// Get the string value (translating escapes) from the ast string.
#[must_use]
pub fn get(string: &ast::String) -> String {
  let text = string.token.text();
  match string.kind {
    ast::StringKind::DoubleQuotedString => double_quoted(text),
    ast::StringKind::SingleQuotedString => single_quoted(text),
    ast::StringKind::DoubleQuotedVerbatimString => double_quoted_verbatim(text),
    ast::StringKind::SingleQuotedVerbatimString => single_quoted_verbatim(text),
    ast::StringKind::TextBlock => {
      let mut sp_st = str_process::St::new(text);
      let mut out = EscapeOutput::new(text);
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

/// Get the string value (translating escapes) from the double-quoted string.
#[must_use]
pub fn double_quoted(text: &str) -> String {
  slash(text, b'"')
}

/// Get the string value (translating escapes) from the single-quoted string.
#[must_use]
pub fn single_quoted(text: &str) -> String {
  slash(text, b'\'')
}

/// Get the string value (translating escapes) from the verbatim double-quoted string.
#[must_use]
pub fn double_quoted_verbatim(text: &str) -> String {
  verbatim(text, b'"')
}

/// Get the string value (translating escapes) from the verbatim single-quoted string.
#[must_use]
pub fn single_quoted_verbatim(text: &str) -> String {
  verbatim(text, b'\'')
}

fn slash(text: &str, delim: u8) -> String {
  let mut sp_st = str_process::St::new(text);
  let mut out = EscapeOutput::new(text);
  if sp_st.cur().is_some_and(|x| x == delim) {
    sp_st.bump();
  } else {
    always!(false, "no {} present", delim.escape_ascii());
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

fn verbatim(text: &str, delim: u8) -> String {
  let mut sp_st = str_process::St::new(text);
  let mut out = EscapeOutput::new(text);
  if sp_st.cur().is_some_and(|x| x == b'@') {
    sp_st.bump();
  } else {
    always!(false, "no @ present");
    return String::new();
  }
  if sp_st.cur().is_some_and(|x| x == delim) {
    sp_st.bump();
  } else {
    always!(false, "no {} present", delim.escape_ascii());
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
