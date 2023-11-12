//! The internal impl.

use crate::error;
use jsonnet_syntax::kind::SyntaxKind as SK;
use str_process::St;

#[allow(clippy::too_many_lines)]
pub(crate) fn token(st: &mut St<'_>, out: &mut error::Output, b: u8) -> SK {
  if is_ws(b) {
    st.bump();
    st.bump_while(is_ws);
    return SK::Whitespace;
  }
  if b == b'#' {
    st.bump();
    st.bump_while(|b| b != b'\n');
    return SK::HashComment;
  }
  if b == b'/' {
    st.bump();
    match st.cur() {
      Some(b'/') => {
        st.bump();
        st.bump_while(|b| b != b'\n');
        return SK::SlashSlashComment;
      }
      Some(b'*') => {
        st.bump();
        let Some(mut prev) = st.cur() else {
          out.err(st.cur_idx(), error::Kind::UnclosedComment);
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
        out.err(st.cur_idx(), error::Kind::UnclosedComment);
        return SK::BlockComment;
      }
      Some(_) | None => return SK::Slash,
    }
  }
  // put this before PUNCTUATION since that contains || and |
  if st.eat_prefix(b"|||") {
    jsonnet_escape::text_block(st, out);
    return SK::TextBlock;
  }
  if let Some(&(_, sk)) = SK::PUNCTUATION.iter().find(|&(bs, _)| st.eat_prefix(bs)) {
    return sk;
  }
  if b.is_ascii_alphabetic() || b == b'_' {
    let start = st.mark();
    st.bump();
    st.bump_while(|b| b.is_ascii_alphanumeric() || b == b'_');
    return SK::keyword(st.non_empty_since(start)).unwrap_or(SK::Id);
  }
  if b.is_ascii_digit() {
    st.bump();
    let m = st.mark();
    st.bump_while(|b| b.is_ascii_digit());
    if st.did_bump_since(m) && b == b'0' {
      out.err(st.cur_idx(), error::Kind::LeadingZero);
    }
    if let Some(b'.') = st.cur() {
      st.bump();
      digits(st, out);
    }
    if let Some(b'e' | b'E') = st.cur() {
      st.bump();
      if let Some(b'-' | b'+') = st.cur() {
        st.bump();
      }
      digits(st, out);
    }
    return SK::Number;
  }
  if b == b'"' {
    st.bump();
    jsonnet_escape::slash(st, out, b'"');
    return SK::DoubleQuotedString;
  }
  if b == b'\'' {
    st.bump();
    jsonnet_escape::slash(st, out, b'\'');
    return SK::SingleQuotedString;
  }
  if b == b'@' {
    st.bump();
    let Some(b) = st.cur() else {
      out.err(st.cur_idx(), error::Kind::InvalidVerbatimDelim);
      return SK::Invalid;
    };
    let ret = match b {
      b'"' => SK::DoubleQuotedVerbatimString,
      b'\'' => SK::SingleQuotedVerbatimString,
      _ => {
        out.err(st.cur_idx(), error::Kind::InvalidVerbatimDelim);
        return SK::Invalid;
      }
    };
    st.bump();
    jsonnet_escape::verbatim(st, out, b);
    return ret;
  }
  // TODO handle more strings
  out.err(st.cur_idx(), error::Kind::InvalidBytes);
  st.next_str();
  SK::Invalid
}

fn digits(st: &mut St<'_>, out: &mut error::Output) {
  let m = st.mark();
  st.bump_while(|b| b.is_ascii_digit());
  if !st.did_bump_since(m) {
    out.err(st.cur_idx(), error::Kind::NeedDigits);
  }
}

fn is_ws(b: u8) -> bool {
  matches!(b, b' ' | b'\t' | b'\n' | b'\r')
}
