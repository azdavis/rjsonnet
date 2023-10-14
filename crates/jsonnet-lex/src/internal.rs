//! The internal impl.

use crate::st::St;
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
    return SK::keyword(st.non_empty_since(start)).unwrap_or(SK::Id);
  }
  if b.is_ascii_digit() {
    st.bump();
    let m = st.mark();
    st.advance_while(|b| b.is_ascii_digit());
    if st.did_advance_since(m) {
      st.err("cannot have a leading 0");
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
  st.err("invalid text");
  st.next_str();
  SK::Invalid
}

fn digits(st: &mut St<'_>) {
  let m = st.mark();
  st.advance_while(|b| b.is_ascii_digit());
  if !st.did_advance_since(m) {
    st.err("need at least 1 digit");
  }
}

fn is_ws(b: u8) -> bool {
  matches!(b, b' ' | b'\t' | b'\n' | b'\r')
}
