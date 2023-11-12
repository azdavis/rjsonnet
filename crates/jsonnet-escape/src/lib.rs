//! Dealing with Jsonnet string escapes.

#![deny(clippy::pedantic, missing_debug_implementations, missing_docs, rust_2018_idioms)]

use std::fmt;
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
}

impl fmt::Display for Error {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Error::NotTerminated => f.write_str("unclosed string"),
      Error::InvalidEscape => f.write_str("invalid escape"),
      Error::NotHexDigit => f.write_str("not a hex digit"),
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
          let minus = if b.is_ascii_digit() {
            b'0'
          } else if b.is_ascii_lowercase() {
            b'a'
          } else if b.is_ascii_uppercase() {
            b'A'
          } else {
            out.err(st.cur_idx(), Error::NotHexDigit);
            continue;
          };
          out.byte(b - minus);
        }
      }
      _ => out.err(st.cur_idx(), Error::InvalidEscape),
    }
  }
  out.err(st.cur_idx(), Error::NotTerminated);
}

/// Handle a verbatim string, ended by the terminator.
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
