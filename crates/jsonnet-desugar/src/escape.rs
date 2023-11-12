use crate::{error, st::St};
use jsonnet_escape::Output;
use jsonnet_syntax::{ast, kind::SyntaxToken};

struct EscapeOutput<'st> {
  bytes: Vec<u8>,
  token: jsonnet_syntax::kind::SyntaxToken,
  st: &'st mut St,
}

impl<'st> EscapeOutput<'st> {
  fn new(token: jsonnet_syntax::kind::SyntaxToken, st: &'st mut St) -> EscapeOutput<'st> {
    // always has at least 2 delimiter bytes (1 at start, 1 at end)
    let cap = token.text().len() - 2;
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
      assert!(sp_st.eat_prefix(b"|||"));
      jsonnet_escape::text_block(&mut sp_st, &mut out);
      String::from_utf8(out.bytes).expect("invalid utf-8 in str")
    }
  }
}

fn slash(st: &mut St, token: SyntaxToken, delim: u8) -> String {
  let mut sp_st = str_process::St::new(token.text());
  let mut out = EscapeOutput::new(token.clone(), st);
  assert_eq!(sp_st.cur().unwrap(), delim);
  sp_st.bump();
  jsonnet_escape::slash(&mut sp_st, &mut out, delim);
  String::from_utf8(out.bytes).expect("invalid utf-8 in str")
}

fn verbatim(st: &mut St, token: SyntaxToken, delim: u8) -> String {
  let mut sp_st = str_process::St::new(token.text());
  let mut out = EscapeOutput::new(token.clone(), st);
  assert_eq!(sp_st.cur().unwrap(), b'@');
  sp_st.bump();
  assert_eq!(sp_st.cur().unwrap(), delim);
  sp_st.bump();
  jsonnet_escape::verbatim(&mut sp_st, &mut out, delim);
  String::from_utf8(out.bytes).expect("invalid utf-8 in str")
}
