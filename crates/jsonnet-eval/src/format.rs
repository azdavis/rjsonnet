//! Rendering values into strings with a parsed formatter.

#![expect(
  dead_code,
  unused_variables,
  clippy::too_many_arguments,
  clippy::fn_params_excessive_bools
)]

use crate::{Cx, error::Result};
use jsonnet_expr::{Expr, Prim, Str};
use jsonnet_format_string::{Code, ConvType, Elem};
use jsonnet_val::jsonnet::{Env, Val};

pub(crate) fn get(cx: &mut Cx<'_>, env: &Env, elems: &[Elem], val: &Val) -> Result<Str> {
  match val {
    Val::Object(obj) => todo!(),
    Val::Array(arr) => todo!(),
    _ => todo!(),
  }
}

fn get_arr(cx: &mut Cx<'_>, elems: &[Elem], exprs: &[(&Env, Expr)]) -> Result<Str> {
  todo!()
}

fn get_one(
  cx: &mut Cx<'_>,
  code: &Code,
  val: Val,
  fw: usize,
  prec: Option<usize>,
  name: &Name,
) -> Result<Str> {
  let cflags = &code.cflags;
  let fpprec = prec.unwrap_or(6);
  let iprec = prec.unwrap_or_default();
  let zp = if cflags.zero && !cflags.left { fw } else { 0 };
  match code.ctype {
    ConvType::D => {
      let Val::Prim(Prim::Number(val)) = val else { todo!("type error") };
      let val = val.value();
      get_int(cx, val <= -1.0, abs_floor(val), zp, iprec, cflags.blank, cflags.plus, 10, false)
    }
    ConvType::O => {
      let Val::Prim(Prim::Number(val)) = val else { todo!("type error") };
      let val = val.value();
      get_int(cx, val <= -1.0, abs_floor(val), zp, iprec, cflags.blank, cflags.plus, 8, cflags.alt)
    }
    ConvType::X(case) => {
      let Val::Prim(Prim::Number(val)) = val else { todo!("type error") };
      /*
        render_hex(std.floor(val), zp, iprec, cflags.blank, cflags.plus, cflags.alt, code.caps)
      */
      todo!("render hex")
    }
    ConvType::E(case) => {
      let Val::Prim(Prim::Number(val)) = val else { todo!("type error") };
      let val = val.value();
      get_float_sci(val, zp, cflags.blank, cflags.plus, cflags.alt, true, case, fpprec)
    }
    ConvType::F(case) => {
      let Val::Prim(Prim::Number(val)) = val else { todo!("type error") };
      let val = val.value();
      get_float_dec(val, zp, cflags.blank, cflags.plus, cflags.alt, true, fpprec)
    }
    ConvType::G(case) => {
      let Val::Prim(Prim::Number(val)) = val else { todo!("type error") };
      let val = val.value();
      let exponent = if val == 0.0 { 0 } else { isize_f64(val.abs().log10().floor()) };
      if exponent < -4 || usize::try_from(exponent).is_ok_and(|e| e >= fpprec) {
        get_float_sci(val, zp, cflags.blank, cflags.plus, cflags.alt, cflags.alt, case, fpprec - 1)
      } else {
        let digits_before_pt = std::cmp::max(1isize, exponent + 1);
        get_float_dec(
          val,
          zp,
          cflags.blank,
          cflags.plus,
          cflags.alt,
          cflags.alt,
          usize_isize(isize_usize(fpprec) - digits_before_pt),
        )
      }
    }
    ConvType::C => match val {
      Val::Prim(Prim::Number(val)) => todo!("std.char(val)"),
      Val::Prim(Prim::String(val)) => {
        /*
        if std.length(val) == 1 then
          val
        else
          error '%c expected 1-sized string got: ' + std.length(val)
          */
        todo!()
      }
      _ => todo!("type error"),
    },
    ConvType::S => crate::exec::str_conv(cx, val),
    ConvType::Percent => todo!(),
  }
}

fn get_float_dec(
  val: f64,
  zp: usize,
  blank: bool,
  plus: bool,
  alt: bool,
  arg: bool,
  fpprec: usize,
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
  case: jsonnet_format_string::Case,
  prec: usize,
) -> Result<Str> {
  todo!()
}

enum Name {
  Number(usize),
  String(Str),
}

fn get_int(
  cx: &mut Cx<'_>,
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
fn isize_f64(n: f64) -> isize {
  n as isize
}

fn usize_isize(n: isize) -> usize {
  match usize::try_from(n) {
    Ok(x) => x,
    Err(e) => todo!("convert: {e}"),
  }
}

fn isize_usize(n: usize) -> isize {
  match isize::try_from(n) {
    Ok(x) => x,
    Err(e) => todo!("convert: {e}"),
  }
}
