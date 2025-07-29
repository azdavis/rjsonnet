//! Format strings used in `std::format` (and thus also `std::mod` and `%`).

#![allow(missing_docs)]

use always::always;
use std::fmt;

/// # Errors
///
/// On parse error.
pub fn get(str: &str) -> Result<Vec<Elem>, ParseError> {
  let mut cx = Cx { bs: str.as_bytes(), idx: 0 };
  let mut out = Vec::<Elem>::new();
  let mut cur = Vec::<u8>::new();
  while let Ok(c) = cx.cur() {
    cx.bump();
    if c != b'%' {
      cur.push(c);
      continue;
    }
    let mkey = if cx.cur()? == b'(' {
      cx.bump();
      let mut v = Vec::<u8>::new();
      loop {
        let c = cx.cur()?;
        if c == b')' {
          cx.bump();
          break match String::from_utf8(v) {
            Ok(x) => Some(x),
            Err(e) => {
              always!(false, "should get UTF-8 from str: {e}");
              None
            }
          };
        }
        v.push(c);
        cx.bump();
      }
    } else {
      None
    };
    let mut cflags = CFlags { alt: false, zero: false, left: false, blank: false, plus: false };
    loop {
      match cx.cur()? {
        b'#' => cflags.alt = true,
        b'0' => cflags.zero = true,
        b'-' => cflags.left = true,
        b' ' => cflags.blank = true,
        b'+' => cflags.plus = true,
        _ => break,
      }
      cx.bump();
    }
    let fw = FieldWidth::try_parse(&mut cx)?;
    let prec = if cx.cur()? == b'.' {
      cx.bump();
      Some(FieldWidth::try_parse(&mut cx)?)
    } else {
      None
    };
    match cx.cur()? {
      b'h' | b'l' | b'L' => cx.bump(),
      _ => {}
    }
    let ctype = match cx.cur()? {
      b'd' | b'i' | b'u' => ConvType::D,
      b'o' => ConvType::O,
      b'x' => ConvType::X(Case::Lower),
      b'X' => ConvType::X(Case::Upper),
      b'e' => ConvType::E(Case::Lower),
      b'E' => ConvType::E(Case::Upper),
      b'f' => ConvType::F(Case::Lower),
      b'F' => ConvType::F(Case::Upper),
      b'g' => ConvType::G(Case::Lower),
      b'G' => ConvType::G(Case::Upper),
      b'c' => ConvType::C,
      b's' => ConvType::S,
      b'%' => {
        cur.push(b'%');
        continue;
      }
      c => return Err(ParseError::UnrecognizedConversionType(c)),
    };
    cx.bump();
    let code = Code { mkey, cflags, fw, prec, ctype };
    push_string(&mut out, cur);
    out.push(Elem::Code(code));
    cur = Vec::new();
  }
  push_string(&mut out, cur);
  Ok(out)
}

fn push_string(out: &mut Vec<Elem>, bs: Vec<u8>) {
  if bs.is_empty() {
    return;
  }
  match String::from_utf8(bs) {
    Ok(x) => out.push(Elem::String(x)),
    Err(e) => {
      always!(false, "should get UTF-8 from str: {e}");
    }
  }
}

#[derive(Debug, Clone)]
pub enum ParseError {
  Truncated,
  UnrecognizedConversionType(u8),
}

impl fmt::Display for ParseError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      ParseError::Truncated => f.write_str("unexpected end of string"),
      ParseError::UnrecognizedConversionType(b) => {
        write!(f, "unrecognized conversion type: '{}'", b.escape_ascii())
      }
    }
  }
}

#[derive(Debug)]
pub enum Elem {
  Code(Code),
  String(String),
}

impl Elem {
  /// Returns the `Code` in this, if any.
  #[must_use]
  pub fn into_code(self) -> Option<Code> {
    match self {
      Elem::Code(code) => Some(code),
      Elem::String(_) => None,
    }
  }
}

#[derive(Debug)]
pub struct Code {
  pub mkey: Option<String>,
  pub cflags: CFlags,
  pub fw: FieldWidth,
  pub prec: Option<FieldWidth>,
  pub ctype: ConvType,
}

#[derive(Debug, Clone, Copy)]
pub enum FieldWidth {
  Star,
  Number(usize),
}

impl FieldWidth {
  fn try_parse(cx: &mut Cx<'_>) -> Result<Self, ParseError> {
    if cx.cur().is_ok_and(|b| b == b'*') {
      cx.bump();
      return Ok(Self::Star);
    }
    let mut v = 0usize;
    loop {
      match cx.cur()? {
        b'0' => v *= 10,
        b'1' => v = v * 10 + 1,
        b'2' => v = v * 10 + 2,
        b'3' => v = v * 10 + 3,
        b'4' => v = v * 10 + 4,
        b'5' => v = v * 10 + 5,
        b'6' => v = v * 10 + 6,
        b'7' => v = v * 10 + 7,
        b'8' => v = v * 10 + 8,
        b'9' => v = v * 10 + 9,
        _ => break Ok(Self::Number(v)),
      }
      cx.bump();
    }
  }
}

#[allow(clippy::struct_excessive_bools)]
#[derive(Debug, Clone, Copy)]
pub struct CFlags {
  pub alt: bool,
  pub zero: bool,
  pub left: bool,
  pub blank: bool,
  pub plus: bool,
}

#[derive(Debug, Clone, Copy)]
pub enum Case {
  Lower,
  Upper,
}

#[derive(Debug, Clone, Copy)]
pub enum ConvType {
  D,
  O,
  X(Case),
  E(Case),
  F(Case),
  G(Case),
  C,
  S,
}

struct Cx<'a> {
  bs: &'a [u8],
  idx: usize,
}

impl Cx<'_> {
  fn cur(&self) -> Result<u8, ParseError> {
    self.bs.get(self.idx).ok_or(ParseError::Truncated).copied()
  }

  fn bump(&mut self) {
    self.idx += 1;
  }
}
