//! The internal impl.

use crate::{error, st::St};
use jsonnet_syntax::kind::SyntaxKind as SK;

pub(crate) fn token(st: &mut St<'_>, b: u8) -> SK {
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
          st.err(error::Kind::UnclosedComment);
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
        st.err(error::Kind::UnclosedComment);
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
    return SK::keyword(st.non_empty_since(start)).unwrap_or(SK::Id);
  }
  if b.is_ascii_digit() {
    st.bump();
    let m = st.mark();
    st.advance_while(|b| b.is_ascii_digit());
    if st.did_advance_since(m) && b == b'0' {
      st.err(error::Kind::LeadingZero);
    }
    if let Some(b'.') = st.cur() {
      st.bump();
      digits(st);
    }
    if let Some(b'e' | b'E') = st.cur() {
      st.bump();
      if let Some(b'-' | b'+') = st.cur() {
        st.bump();
      }
      digits(st);
    }
    return SK::Number;
  }
  if b == b'"' {
    st.bump();
    jsonnet_escape::get(st, b'"');
    return SK::DoubleQuotedString;
  }
  // TODO handle more strings
  st.err(error::Kind::InvalidBytes);
  st.next_str();
  SK::Invalid
}

fn digits(st: &mut St<'_>) {
  let m = st.mark();
  st.advance_while(|b| b.is_ascii_digit());
  if !st.did_advance_since(m) {
    st.err(error::Kind::NeedDigits);
  }
}

fn is_ws(b: u8) -> bool {
  matches!(b, b' ' | b'\t' | b'\n' | b'\r')
}
