//! Display types.

use super::{Data, Fn, GlobalStore, LocalStore, Param, Prim, Ty};
use always::always;
use jsonnet_expr::StrArena;
use std::fmt::{self, Write as _};

impl Ty {
  /// Displays a type.
  ///
  /// Meant to be somewhat similar to how TypeScript does it.
  ///
  /// NOTE: in most cases, you'll pass `None` for the local store, because local stores are meant to
  /// be immediately combined into the global store. But we allow passing `Some` for debugging
  /// purposes.
  #[must_use]
  pub fn display<'a>(
    self,
    multi_line: MultiLine,
    global: &'a GlobalStore,
    local: Option<&'a LocalStore>,
    str_ar: &'a StrArena,
  ) -> impl fmt::Display + 'a {
    let level = match multi_line {
      MultiLine::MustNot => None,
      MultiLine::May => Some(0),
    };
    TyDisplay { ty: self, prec: Prec::Min, stuff: Stuff { global, local, str_ar, level } }
  }
}

impl Fn {
  /// Displays a function for signature help.
  ///
  /// # Errors
  ///
  /// When we couldn't display things (a bug).
  pub fn display_for_sig_help<'a>(
    &self,
    global: &'a GlobalStore,
    local: Option<&'a LocalStore>,
    str_ar: &'a StrArena,
  ) -> Result<(String, Vec<std::ops::Range<u32>>), fmt::Error> {
    let (params, ret) = self.parts();
    let mut param_ranges = Vec::<std::ops::Range<u32>>::new();
    // NOTE: some duplication with `FnDisplay`.
    let mut buf = "(".to_owned();
    if let Some(params) = params {
      let mut first = true;
      for &param in params {
        if first {
          first = false;
        } else {
          buf.push_str(", ");
        }
        let start = always::convert::usize_to_u32(buf.len());
        let param_d = ParamDisplay { param, stuff: Stuff { global, local, str_ar, level: None } };
        write!(&mut buf, "{param_d}")?;
        let end = always::convert::usize_to_u32(buf.len());
        param_ranges.push(start..end);
      }
      always!(params.len() == param_ranges.len());
    } else {
      buf.push_str("...");
    }
    buf.push_str(") => ");
    let ret = ret.display(MultiLine::MustNot, global, local, str_ar);
    write!(&mut buf, "{ret}")?;
    Ok((buf, param_ranges))
  }
}

/// Whether the type can be displayed multi-line.
#[derive(Debug, Clone, Copy, Default)]
pub enum MultiLine {
  /// It may never be.
  MustNot,
  /// It may sometimes be.
  #[default]
  May,
}

impl MultiLine {
  const THRESHOLD: usize = 3;
}

fn increase_level(n: usize, lv: Option<usize>) -> [Option<usize>; 3] {
  match lv {
    None => [None; 3],
    Some(x) => {
      if n > MultiLine::THRESHOLD {
        [Some(x), Some(x + 1), Some(x + 1)]
      } else {
        [None, None, Some(x)]
      }
    }
  }
}

#[derive(Clone, Copy)]
struct TyDisplay<'a> {
  ty: Ty,
  prec: Prec,
  stuff: Stuff<'a>,
}

impl TyDisplay<'_> {
  fn with(self, ty: Ty, prec: Prec) -> Self {
    Self { ty, prec, ..self }
  }
}

impl fmt::Display for TyDisplay<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let data =
      self.stuff.global.0.data(self.ty, false).or_else(|| self.stuff.local?.0.data(self.ty, true));
    let Some(data) = data else {
      always!(false, "not in store: {:?}", self.ty);
      return f.write_str("_");
    };
    match data {
      Data::Prim(prim) => match prim {
        Prim::Any => f.write_str("any"),
        Prim::True => f.write_str("true"),
        Prim::False => f.write_str("false"),
        Prim::Null => f.write_str("null"),
        Prim::String => f.write_str("string"),
        Prim::Number => f.write_str("number"),
      },
      Data::Array(ty) => {
        self.with(*ty, Prec::Array).fmt(f)?;
        f.write_str("[]")
      }
      Data::Object(obj) => {
        let [cur_level, new_level, rec_level] = increase_level(obj.known.len(), self.stuff.level);
        let mut iter = obj.known.iter().map(|(key, ty)| FieldDisplay {
          key,
          ty: *ty,
          stuff: Stuff { level: rec_level, ..self.stuff },
        });
        if let Some(field) = iter.next() {
          f.write_str("{")?;
          field_sep(f, new_level)?;
          field.fmt(f)?;
          for field in iter {
            f.write_str(",")?;
            field_sep(f, new_level)?;
            field.fmt(f)?;
          }
          if obj.has_unknown {
            f.write_str(",")?;
            field_sep(f, new_level)?;
            f.write_str("...")?;
          }
          // trailing comma iff multi line
          if cur_level.is_some() {
            f.write_str(",")?;
          }
          field_sep(f, cur_level)?;
          f.write_str("}")
        } else if obj.has_unknown {
          f.write_str("object")
        } else {
          f.write_str("{}")
        }
      }
      Data::Fn(func) => {
        let (params, ret) = func.parts();
        FnDisplay { params, ret, prec: self.prec, stuff: self.stuff }.fmt(f)
      }
      Data::Union(tys) => {
        let has_bool = tys.contains(&Ty::TRUE) && tys.contains(&Ty::FALSE);
        let mut iter = tys.iter().filter_map(|&ty| {
          if has_bool && (ty == Ty::TRUE || ty == Ty::FALSE) {
            None
          } else {
            Some(self.with(ty, Prec::Union))
          }
        });
        let Some(ty) = iter.next() else {
          return f.write_str(if has_bool { "boolean" } else { "never" });
        };
        let needs_paren = self.prec > Prec::Union;
        if needs_paren {
          f.write_str("(")?;
        }
        if has_bool {
          f.write_str("boolean | ")?;
        }
        ty.fmt(f)?;
        for ty in iter {
          f.write_str(" | ")?;
          ty.fmt(f)?;
        }
        if needs_paren {
          f.write_str(")")?;
        }
        Ok(())
      }
    }
  }
}

#[derive(Clone, Copy)]
struct FnDisplay<'a> {
  params: Option<&'a [Param]>,
  ret: Ty,
  prec: Prec,
  stuff: Stuff<'a>,
}

impl fmt::Display for FnDisplay<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let needs_paren = self.prec > Prec::Min;
    if needs_paren {
      f.write_str("(")?;
    }
    f.write_str("(")?;
    if let Some(params) = self.params {
      let [cur_level, new_level, rec_level] = increase_level(params.len(), self.stuff.level);
      let mut iter = params
        .iter()
        .map(|&param| ParamDisplay { param, stuff: Stuff { level: rec_level, ..self.stuff } });
      if let Some(new_level) = new_level {
        nl_indent(f, new_level)?;
      }
      if let Some(p) = iter.next() {
        p.fmt(f)?;
      }
      for p in iter {
        f.write_str(",")?;
        field_sep(f, new_level)?;
        p.fmt(f)?;
      }
      if let Some(cur_level) = cur_level {
        f.write_str(",")?;
        nl_indent(f, cur_level)?;
      }
    } else {
      f.write_str("...")?;
    }
    f.write_str(") => ")?;
    TyDisplay { ty: self.ret, prec: self.prec, stuff: self.stuff }.fmt(f)?;
    if needs_paren {
      f.write_str(")")?;
    }
    Ok(())
  }
}

#[derive(Clone, Copy)]
struct FieldDisplay<'a> {
  key: &'a jsonnet_expr::Str,
  ty: Ty,
  stuff: Stuff<'a>,
}

impl fmt::Display for FieldDisplay<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    self.stuff.str_ar.get(self.key).fmt(f)?;
    f.write_str(": ")?;
    TyDisplay { ty: self.ty, prec: Prec::Min, stuff: self.stuff }.fmt(f)
  }
}

#[derive(Clone, Copy)]
struct ParamDisplay<'a> {
  param: Param,
  stuff: Stuff<'a>,
}

impl fmt::Display for ParamDisplay<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    self.param.id.display(self.stuff.str_ar).fmt(f)?;
    if !self.param.required {
      f.write_str("?")?;
    }
    f.write_str(": ")?;
    TyDisplay { ty: self.param.ty, prec: Prec::Min, stuff: self.stuff }.fmt(f)?;
    Ok(())
  }
}

/// Just a bunch of common stuff we need in a few places. Naming is hard.
#[derive(Clone, Copy)]
struct Stuff<'a> {
  global: &'a GlobalStore,
  local: Option<&'a LocalStore>,
  str_ar: &'a StrArena,
  level: Option<usize>,
}

fn field_sep(f: &mut fmt::Formatter<'_>, level: Option<usize>) -> fmt::Result {
  match level {
    None => f.write_str(" "),
    Some(level) => nl_indent(f, level),
  }
}

const INDENT: &str = "  ";

fn nl_indent(f: &mut fmt::Formatter<'_>, level: usize) -> fmt::Result {
  f.write_str("\n")?;
  for _ in 0..level {
    f.write_str(INDENT)?;
  }
  Ok(())
}

/// Precedence when printing a type.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Prec {
  Min,
  Union,
  Array,
}
