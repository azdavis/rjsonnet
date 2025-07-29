//! Rendering values into strings with a parsed formatter.

#![expect(
  dead_code,
  unused_variables,
  clippy::too_many_arguments,
  clippy::fn_params_excessive_bools
)]

use jsonnet_expr::{Prim, Str, StrArena};
use jsonnet_format_parse::{Case, Code, ConvType, Elem};
use jsonnet_val::json::Val;
use std::fmt;

/// std's result specialized to default to [`Error`].
pub type Result<T, E = Error> = std::result::Result<T, E>;

/// Render the val against the elems.
///
/// # Errors
///
/// When that failed.
pub fn get(ar: &mut StrArena, elems: &[Elem], val: &Val) -> Result<Str> {
  Err(Error::Todo)
}

fn get_arr(ar: &mut StrArena, elems: &[Elem], vals: &[Val]) -> Result<Str> {
  todo!()
}

fn get_one(
  ar: &mut StrArena,
  code: &Code,
  val: &Val,
  fw: usize,
  prec: Option<usize>,
  name: &Name,
) -> Result<Str> {
  let cflags = &code.cflags;
  let fp_prec = prec.unwrap_or(6);
  let i_prec = prec.unwrap_or_default();
  let zp = if cflags.zero && !cflags.left { fw } else { 0 };
  match code.ctype {
    ConvType::D => {
      let Val::Prim(Prim::Number(val)) = val else { todo!("type error") };
      let val = val.value();
      get_int(ar, val <= -1.0, abs_floor(val), zp, i_prec, cflags.blank, cflags.plus, 10, false)
    }
    ConvType::O => {
      let Val::Prim(Prim::Number(val)) = val else { todo!("type error") };
      let val = val.value();
      get_int(ar, val <= -1.0, abs_floor(val), zp, i_prec, cflags.blank, cflags.plus, 8, cflags.alt)
    }
    ConvType::X(case) => {
      let Val::Prim(Prim::Number(val)) = val else { todo!("type error") };
      let val = val.value();
      let val_floor = f64_to_isize(val.floor());
      let mut n = val_floor.unsigned_abs();
      let mut cs = Vec::<char>::new();
      if n == 0 {
        cs.push('0');
      } else {
        while n != 0 {
          cs.push(digit_to_char(n % 16, case));
          n /= 16;
        }
      }
      let neg = val_floor < 0;
      let zero_pad_len = std::cmp::max(
        i_prec,
        zp - usize::from(neg || cflags.blank || cflags.plus) - (if cflags.alt { 2 } else { 0 }),
      );
      while cs.len() < zero_pad_len {
        cs.push('0');
      }
      if cflags.alt {
        let x = match case {
          Case::Lower => 'x',
          Case::Upper => 'X',
        };
        cs.push(x);
        cs.push('0');
      }
      if neg {
        cs.push('-');
      } else if cflags.plus {
        cs.push('+');
      } else if cflags.blank {
        cs.push(' ');
      }
      cs.reverse();
      Ok(ar.str(cs.into_iter().collect()))
    }
    ConvType::E(case) => {
      let Val::Prim(Prim::Number(val)) = val else { todo!("type error") };
      let val = val.value();
      get_float_sci(val, zp, cflags.blank, cflags.plus, cflags.alt, true, case, fp_prec)
    }
    ConvType::F(case) => {
      let Val::Prim(Prim::Number(val)) = val else { todo!("type error") };
      let val = val.value();
      get_float_dec(val, zp, cflags.blank, cflags.plus, cflags.alt, true, fp_prec)
    }
    ConvType::G(case) => {
      let Val::Prim(Prim::Number(val)) = val else { todo!("type error") };
      let val = val.value();
      let exponent = if val == 0.0 { 0 } else { f64_to_isize(val.abs().log10().floor()) };
      if exponent < -4 || usize::try_from(exponent).is_ok_and(|e| e >= fp_prec) {
        get_float_sci(val, zp, cflags.blank, cflags.plus, cflags.alt, cflags.alt, case, fp_prec - 1)
      } else {
        let digits_before_pt = std::cmp::max(1isize, exponent + 1);
        get_float_dec(
          val,
          zp,
          cflags.blank,
          cflags.plus,
          cflags.alt,
          cflags.alt,
          isize_to_usize(usize_to_isize(fp_prec) - digits_before_pt),
        )
      }
    }
    ConvType::C => match val {
      Val::Prim(Prim::Number(val)) => todo!("std.char(val)"),
      Val::Prim(Prim::String(val)) => {
        let len = ar.get(*val).chars().count();
        if len == 1 { Ok(*val) } else { todo!("%c expected 1-sized string got: {len}") }
      }
      _ => todo!("type error"),
    },
    ConvType::S => {
      let string = val.display(ar).to_string();
      Ok(ar.str(string.into_boxed_str()))
    }
  }
}

fn get_float_dec(
  val: f64,
  zp: usize,
  blank: bool,
  plus: bool,
  alt: bool,
  arg: bool,
  fp_prec: usize,
) -> Result<Str> {
  todo!()
}

fn get_float_sci(
  val: f64,
  zp: usize,
  blank: bool,
  plus: bool,
  ensure_pt: bool,
  trailing: bool,
  case: Case,
  prec: usize,
) -> Result<Str> {
  todo!()
}

enum Name {
  Number(usize),
  String(Str),
}

fn get_int(
  ar: &mut StrArena,
  neg: bool,
  mag: usize,
  min_chars: usize,
  min_digits: usize,
  blank: bool,
  plus: bool,
  radix: usize,
  zero_prefix: bool,
) -> Result<Str> {
  todo!()
}

#[expect(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
fn abs_floor(n: f64) -> usize {
  n.abs().floor() as usize
}

#[expect(clippy::cast_possible_truncation)]
fn f64_to_isize(n: f64) -> isize {
  n as isize
}

fn isize_to_usize(n: isize) -> usize {
  match usize::try_from(n) {
    Ok(x) => x,
    Err(e) => todo!("convert: {e}"),
  }
}

fn usize_to_isize(n: usize) -> isize {
  match isize::try_from(n) {
    Ok(x) => x,
    Err(e) => todo!("convert: {e}"),
  }
}

fn digit_to_char(n: usize, case: Case) -> char {
  match (n, case) {
    (0, _) => '0',
    (1, _) => '1',
    (2, _) => '2',
    (3, _) => '3',
    (4, _) => '4',
    (5, _) => '5',
    (6, _) => '6',
    (7, _) => '7',
    (8, _) => '8',
    (9, _) => '9',
    (10, Case::Lower) => 'a',
    (10, Case::Upper) => 'A',
    (11, Case::Lower) => 'b',
    (11, Case::Upper) => 'B',
    (12, Case::Lower) => 'c',
    (12, Case::Upper) => 'C',
    (13, Case::Lower) => 'd',
    (13, Case::Upper) => 'D',
    (14, Case::Lower) => 'e',
    (14, Case::Upper) => 'E',
    (15, Case::Lower) => 'f',
    (15, Case::Upper) => 'F',
    _ => panic!("not a digit: {n}"),
  }
}

/// An error when formatting.
#[derive(Debug, Clone)]
pub enum Error {
  /// Not yet implemented.
  Todo,
}

impl fmt::Display for Error {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Error::Todo => f.write_str("not yet implemented"),
    }
  }
}
