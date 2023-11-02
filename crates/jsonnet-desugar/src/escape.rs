use crate::{error, st::St};
use jsonnet_syntax::{ast, kind::SyntaxToken};

struct EscapeSt<'str, 'st> {
  bytes: std::iter::Peekable<std::str::Bytes<'str>>,
  out: Vec<u8>,
  token: jsonnet_syntax::kind::SyntaxToken,
  st: &'st mut St,
}

impl<'str, 'st> Iterator for EscapeSt<'str, 'st> {
  type Item = u8;

  fn next(&mut self) -> Option<Self::Item> {
    self.bytes.next()
  }
}

impl<'str, 'st> jsonnet_escape::State for EscapeSt<'str, 'st> {
  fn peek(&mut self) -> Option<u8> {
    self.bytes.peek().copied()
  }

  fn err(&mut self, e: jsonnet_escape::Error) {
    self.st.err_token(self.token.clone(), error::Kind::Escape(e));
  }

  fn output(&mut self, b: u8) {
    self.out.push(b);
  }
}

pub(crate) fn get(st: &mut St, string: ast::String) -> String {
  match string.kind {
    ast::StringKind::DoubleQuotedString => slash(st, string.token, b'"'),
    ast::StringKind::SingleQuotedString => slash(st, string.token, b'\''),
    ast::StringKind::DoubleQuotedVerbatimString => verbatim(st, string.token, b'"'),
    ast::StringKind::SingleQuotedVerbatimString => verbatim(st, string.token, b'\''),
    ast::StringKind::TextBlock => todo!(),
  }
}

fn slash(st: &mut St, token: SyntaxToken, delim: u8) -> String {
  let text = token.text();
  let mut escape_st = EscapeSt {
    bytes: text.bytes().peekable(),
    st,
    token: token.clone(),
    out: Vec::with_capacity(text.len() - 2),
  };
  assert_eq!(escape_st.next().unwrap(), delim);
  jsonnet_escape::slash(&mut escape_st, delim);
  String::from_utf8(escape_st.out).expect("invalid utf-8 in str")
}

fn verbatim(st: &mut St, token: SyntaxToken, delim: u8) -> String {
  let text = token.text();
  let mut escape_st = EscapeSt {
    bytes: text.bytes().peekable(),
    st,
    token: token.clone(),
    out: Vec::with_capacity(text.len() - 3),
  };
  assert_eq!(escape_st.next().unwrap(), b'@');
  assert_eq!(escape_st.next().unwrap(), delim);
  jsonnet_escape::verbatim(&mut escape_st, delim);
  String::from_utf8(escape_st.out).expect("invalid utf-8 in str")
}
