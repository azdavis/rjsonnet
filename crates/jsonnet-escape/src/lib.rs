//! Dealing with string escapes.

use std::fmt::{self, Write as _};
use str_process::St;

/// An error when interpreting the escaped bytes.
#[derive(Debug)]
pub enum Error {
  /// The bytes were not terminated by the given terminator.
  NotTerminated,
  /// There was an invalid escape.
  InvalidEscape,
  /// One of the `D` in the `\uDDDD` escape was not a hex digit.
  NotHexDigit,
  /// There was no new line on the starting `|||` line for a text block.
  NoNewLineForTextBlockStart,
  /// A content line of a text block didn't start with the whitespace prefix.
  NoWhitespacePrefixForTextBlockFirstLine,
  /// There was no closing `|||` on the ending line for a text block.
  NoBarBarBarForTextBlockEnd,
}

impl fmt::Display for Error {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Error::NotTerminated => f.write_str("unclosed string"),
      Error::InvalidEscape => f.write_str("invalid escape"),
      Error::NotHexDigit => f.write_str("not a hex digit"),
      Error::NoNewLineForTextBlockStart => {
        f.write_str("must have a newline after `|||` to start text block")
      }
      Error::NoWhitespacePrefixForTextBlockFirstLine => {
        f.write_str("first line of text block must start with whitespace")
      }
      Error::NoBarBarBarForTextBlockEnd => f.write_str("must have a `|||` to end text block"),
    }
  }
}

/// The state for turning a sequence of escaped bytes into a sequence of interpreted bytes.
pub trait Output {
  /// Records an error at the index.
  fn err(&mut self, idx: usize, e: Error);
  /// Outputs an interpreted byte.
  fn byte(&mut self, b: u8);
}

/// Handle a string that contains slash escapes, like `\n`, ended by the terminator.
///
/// Before this function, we have already seen the opening quote (which is the same as the
/// terminator).
pub fn slash<O>(st: &mut St<'_>, out: &mut O, terminator: u8)
where
  O: Output,
{
  while let Some(cur) = st.cur() {
    st.bump();
    if cur == terminator {
      return;
    }
    if cur != b'\\' {
      out.byte(cur);
      continue;
    }
    let Some(b) = st.cur() else { break };
    st.bump();
    match b {
      b'"' => out.byte(b'"'),
      b'\'' => out.byte(b'\''),
      b'\\' => out.byte(b'\\'),
      b'/' => out.byte(b'/'),
      b'b' => out.byte(8),
      b'f' => out.byte(12),
      b'n' => out.byte(10),
      b'r' => out.byte(13),
      b't' => out.byte(9),
      b'u' => {
        for _ in 0..4 {
          let Some(b) = st.cur() else { break };
          st.bump();
          let off = if b.is_ascii_digit() {
            b'0'
          } else if b.is_ascii_lowercase() {
            b'a'
          } else if b.is_ascii_uppercase() {
            b'A'
          } else {
            out.err(st.cur_idx(), Error::NotHexDigit);
            continue;
          };
          out.byte(b - off);
        }
      }
      _ => out.err(st.cur_idx(), Error::InvalidEscape),
    }
  }
  out.err(st.cur_idx(), Error::NotTerminated);
}

/// Handle a verbatim string, ended by the terminator.
///
/// Before this function, we have already seen the opening @ and delimiter character (which is the
/// same as the terminator).
///
/// Two consecutive terminators do not terminate the string, but are interpreted as one terminator
/// character in the string.
pub fn verbatim<O>(st: &mut St<'_>, out: &mut O, terminator: u8)
where
  O: Output,
{
  while let Some(cur) = st.cur() {
    st.bump();
    if cur == terminator {
      if st.cur().is_some_and(|x| x == terminator) {
        st.bump();
      } else {
        return;
      }
    }
    out.byte(cur);
  }
  out.err(st.cur_idx(), Error::NotTerminated);
}

/// Handle a text block
///
/// Before this function, we have already seen the opening `|||`.
pub fn text_block<O>(st: &mut St<'_>, out: &mut O)
where
  O: Output,
{
  let remove_final_nl = st.cur().is_some_and(|b| b == b'-');
  if remove_final_nl {
    st.bump();
  }
  st.bump_while(is_non_nl_ws);
  if st.cur().is_some_and(|b| b == b'\n') {
    st.bump();
  } else {
    out.err(st.cur_idx(), Error::NoNewLineForTextBlockStart);
  }
  let prefix_start = st.mark();
  st.bump_while(is_non_nl_ws);
  let prefix = st.since(prefix_start);
  if prefix.is_empty() {
    out.err(st.cur_idx(), Error::NoWhitespacePrefixForTextBlockFirstLine);
    give_up_on_text_block(st);
    return;
  }
  let mut pending_nl = false;
  loop {
    if st.cur().is_none() {
      out.err(st.cur_idx(), Error::NoBarBarBarForTextBlockEnd);
      break;
    }
    st.bump_while(|b| {
      let not_nl = b != b'\n';
      if not_nl {
        if pending_nl {
          out.byte(b'\n');
          pending_nl = false;
        }
        out.byte(b);
      }
      not_nl
    });
    if st.cur().is_some_and(|b| b == b'\n') {
      st.bump();
      if pending_nl {
        out.byte(b'\n');
      }
      pending_nl = true;
      if st.eat_prefix(prefix) {
        continue;
      }
    }
    st.bump_while(is_non_nl_ws);
    if st.eat_prefix(b"|||") {
      break;
    }
  }
  if pending_nl && !remove_final_nl {
    out.byte(b'\n');
  }
}

fn is_non_nl_ws(b: u8) -> bool {
  matches!(b, b' ' | b'\t' | b'\r')
}

fn give_up_on_text_block(st: &mut St<'_>) {
  while st.cur().is_some() && !st.eat_prefix(b"|||") {
    st.bump();
  }
}

/// A wrapper for unescaping a byte slice via its [`fmt::Display`] impl.
///
/// When displayed, it will have `""` around the unescaped bytes.
#[derive(Debug)]
#[repr(transparent)]
pub struct Unescape {
  s: str,
}

impl Unescape {
  /// Creates a new one of these.
  #[must_use]
  #[expect(unsafe_code)]
  pub fn new(s: &str) -> &Self {
    let ptr = s as *const str;
    let ptr = ptr as *const Unescape;
    // SAFETY: Unescape transparently wraps str,
    // and &*s is &str.
    unsafe { &*ptr }
  }
}

#[expect(clippy::disallowed_methods)]
mod chars {
  pub(crate) const B: char = char::from_u32(0x8).expect("ASCII");
  pub(crate) const F: char = char::from_u32(0xC).expect("ASCII");
  pub(crate) const N: char = char::from_u32(0xA).expect("ASCII");
  pub(crate) const R: char = char::from_u32(0xD).expect("ASCII");
  pub(crate) const T: char = char::from_u32(0x9).expect("ASCII");
}

impl fmt::Display for Unescape {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.write_str("\"")?;
    for c in self.s.chars() {
      match c {
        '"' => f.write_str("\\\"")?,
        '\\' => f.write_str("\\\\")?,
        chars::B => f.write_str("\\b")?,
        chars::F => f.write_str("\\f")?,
        chars::N => f.write_str("\\n")?,
        chars::R => f.write_str("\\r")?,
        chars::T => f.write_str("\\t")?,
        _ => f.write_char(c)?,
      }
    }
    f.write_str("\"")
  }
}
