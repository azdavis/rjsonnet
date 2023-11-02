//! Dealing with Jsonnet string escapes.

#![deny(clippy::pedantic, missing_debug_implementations, rust_2018_idioms)]

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
///
/// It is an Iterator over the escaped bytes.
pub trait State: Iterator<Item = u8> {
  /// Record an error at the current position, i.e. the most recent escaped byte the state examined.
  fn err(&mut self, e: Error);
  /// Output an interpreted byte.
  fn output(&mut self, b: u8);
}

pub fn slash<S>(st: &mut S, terminator: u8)
where
  S: State,
{
  while let Some(cur) = st.next() {
    if cur == terminator {
      return;
    }
    if cur != b'\\' {
      st.output(cur);
      continue;
    }
    let Some(b) = st.next() else { break };
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
          let Some(b) = st.next() else { break };
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
