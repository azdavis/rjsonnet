//! The internal impl.

use crate::{error, st::St};
use jsonnet_escape::State as _;
use jsonnet_syntax::kind::SyntaxKind as SK;

#[allow(clippy::too_many_lines)]
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
  // put this before PUNCTUATION since that contains || and |
  if st.eat_prefix(b"|||") {
    st.advance_while(is_non_nl_ws);
    if st.cur().is_some_and(|b| b == b'\n') {
      st.bump();
    } else {
      st.err(error::Kind::NoNewLineForTextBlockStart);
    }
    let prefix_start = st.mark();
    st.advance_while(is_non_nl_ws);
    let prefix = st.since(prefix_start);
    if prefix.is_empty() {
      st.err(error::Kind::NoWhitespacePrefixForTextBlockFirstLine);
    }
    st.advance_while(|b| b != b'\n');
    st.bump();
    loop {
      if st.eat_prefix(prefix) {
        st.advance_while(|b| b != b'\n');
      }
      if st.cur().is_some_and(|b| b == b'\n') {
        st.bump();
        continue;
      }
      st.advance_while(is_non_nl_ws);
      if !st.eat_prefix(b"|||") {
        st.err(error::Kind::NoBarBarBarForTextBlockEnd);
      }
      break;
    }
    return SK::TextBlock;
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
    jsonnet_escape::slash(st, b'"');
    return SK::DoubleQuotedString;
  }
  if b == b'\'' {
    st.bump();
    jsonnet_escape::slash(st, b'\'');
    return SK::SingleQuotedString;
  }
  if b == b'@' {
    st.bump();
    let Some(b) = st.cur() else {
      st.err(error::Kind::InvalidVerbatimDelim);
      return SK::Invalid;
    };
    let ret = match b {
      b'"' => SK::DoubleQuotedVerbatimString,
      b'\'' => SK::SingleQuotedVerbatimString,
      _ => {
        st.err(error::Kind::InvalidVerbatimDelim);
        return SK::Invalid;
      }
    };
    st.bump();
    jsonnet_escape::verbatim(st, b);
    return ret;
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

fn is_non_nl_ws(b: u8) -> bool {
  matches!(b, b' ' | b'\t' | b'\r')
}
