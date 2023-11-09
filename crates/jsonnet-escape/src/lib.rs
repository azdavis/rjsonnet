//! Dealing with Jsonnet string escapes.

#![deny(clippy::pedantic, missing_debug_implementations, missing_docs, rust_2018_idioms)]

use std::fmt;

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
pub trait State {
  /// Returns the current byte without advancing.
  fn cur(&mut self) -> Option<u8>;
  /// Advances to the next byte.
  fn bump(&mut self);
  /// Records an error at the byte that would be returned by `cur`.
  fn err(&mut self, e: Error);
  /// Outputs an interpreted byte.
  fn output(&mut self, b: u8);
}

/// Handle a string that contains slash escapes, like `\n`, ended by the terminator.
pub fn slash<S>(st: &mut S, terminator: u8)
where
  S: State,
{
  while let Some(cur) = st.cur() {
    st.bump();
    if cur == terminator {
      return;
    }
    if cur != b'\\' {
      st.output(cur);
      continue;
    }
    let Some(b) = st.cur() else { break };
    st.bump();
    match b {
      b'"' => st.output(b'"'),
      b'\'' => st.output(b'\''),
      b'\\' => st.output(b'\\'),
      b'/' => st.output(b'/'),
      b'b' => st.output(8),
      b'f' => st.output(12),
      b'n' => st.output(10),
      b'r' => st.output(13),
      b't' => st.output(9),
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
            st.err(Error::NotHexDigit);
            continue;
          };
          st.output(b - minus);
        }
      }
      _ => st.err(Error::InvalidEscape),
    }
  }
  st.err(Error::NotTerminated);
}

/// Handle a verbatim string, ended by the terminator.
///
/// Two consecutive terminators do not terminate the string, but are interpreted as one terminator
/// character in the string.
pub fn verbatim<S>(st: &mut S, terminator: u8)
where
  S: State,
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
    st.output(cur);
  }
  st.err(Error::NotTerminated);
}
