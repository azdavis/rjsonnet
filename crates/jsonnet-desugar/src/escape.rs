use crate::{error, st::St};
use always::always;
use jsonnet_escape::Output;
use jsonnet_syntax::{ast, kind::SyntaxToken};

struct EscapeOutput<'st> {
  bytes: Vec<u8>,
  token: jsonnet_syntax::kind::SyntaxToken,
  st: &'st mut St,
}

impl<'st> EscapeOutput<'st> {
  fn new(token: jsonnet_syntax::kind::SyntaxToken, st: &'st mut St) -> EscapeOutput<'st> {
    // usually has at least 2 delimiter bytes (1 at start, 1 at end), but may not if the token was
    // malformed (lex/parse error)
    let cap = token.text().len().saturating_sub(2);
    EscapeOutput { bytes: Vec::with_capacity(cap), token, st }
  }
}

impl<'st> Output for EscapeOutput<'st> {
  fn err(&mut self, _: usize, e: jsonnet_escape::Error) {
    self.st.err_token(self.token.clone(), error::Kind::Escape(e));
  }

  fn byte(&mut self, b: u8) {
    self.bytes.push(b);
  }
}

pub(crate) fn get(st: &mut St, string: ast::String) -> String {
  match string.kind {
    ast::StringKind::DoubleQuotedString => slash(st, string.token, b'"'),
    ast::StringKind::SingleQuotedString => slash(st, string.token, b'\''),
    ast::StringKind::DoubleQuotedVerbatimString => verbatim(st, string.token, b'"'),
    ast::StringKind::SingleQuotedVerbatimString => verbatim(st, string.token, b'\''),
    ast::StringKind::TextBlock => {
      let mut sp_st = str_process::St::new(string.token.text());
      let mut out = EscapeOutput::new(string.token.clone(), st);
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

fn slash(st: &mut St, token: SyntaxToken, delim: u8) -> String {
  let mut sp_st = str_process::St::new(token.text());
  let mut out = EscapeOutput::new(token.clone(), st);
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

fn verbatim(st: &mut St, token: SyntaxToken, delim: u8) -> String {
  let mut sp_st = str_process::St::new(token.text());
  let mut out = EscapeOutput::new(token.clone(), st);
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
