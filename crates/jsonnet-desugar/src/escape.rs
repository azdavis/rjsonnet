use crate::{error, st::St};
use jsonnet_syntax::ast;

struct EscapeSt<'str, 'st> {
  bytes: std::str::Bytes<'str>,
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
  fn err(&mut self, e: jsonnet_escape::Error) {
    self.st.err_token(self.token.clone(), error::Kind::Escape(e));
  }

  fn output(&mut self, b: u8) {
    self.out.push(b);
  }
}

pub(crate) fn get(st: &mut St, string: &ast::String) -> String {
  let delim = match string.kind {
    ast::StringKind::DoubleQuotedString => b'"',
    ast::StringKind::SingleQuotedString => b'\'',
    _ => todo!(),
  };
  let text = string.token.text().strip_prefix(char::from(delim)).expect("couldn't strip prefix");
  let mut escape_st = EscapeSt {
    bytes: text.bytes(),
    st,
    token: string.token.clone(),
    out: Vec::with_capacity(text.len()),
  };
  jsonnet_escape::get(&mut escape_st, delim);
  String::from_utf8(escape_st.out).expect("invalid utf-8 in str")
}
