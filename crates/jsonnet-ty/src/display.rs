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
    style: Style,
    global: &'a GlobalStore,
    local: Option<&'a LocalStore>,
    str_ar: &'a StrArena,
  ) -> impl fmt::Display {
    let stores = Stores { global, local };
    let level = match style {
      Style::Short => None,
      Style::Long => Some(0),
    };
    TyDisplay { ty: self, prec: Prec::Min, stuff: Stuff { stores, str_ar, level } }
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
    let stores = Stores { global, local };
    // NOTE: some duplication with `FnDisplay`.
    let Some((params, ret)) = self.parts() else { return Ok(("function".to_owned(), Vec::new())) };
    let mut param_ranges = Vec::<std::ops::Range<u32>>::new();
    let mut buf = "(".to_owned();
    let mut first = true;
    for &param in params {
      if first {
        first = false;
      } else {
        buf.push_str(", ");
      }
      let start = always::convert::usize_to_u32(buf.len());
      let param_d = ParamDisplay { param, stuff: Stuff { stores, str_ar, level: None } };
      write!(&mut buf, "{param_d}")?;
      let end = always::convert::usize_to_u32(buf.len());
      param_ranges.push(start..end);
    }
    always!(params.len() == param_ranges.len());
    buf.push_str(") => ");
    let ret = ret.display(Style::Short, global, local, str_ar);
    write!(&mut buf, "{ret}")?;
    Ok((buf, param_ranges))
  }
}

/// The style for display.
#[derive(Debug, Clone, Copy, Default)]
pub enum Style {
  /// Short, compact style.
  Short,
  /// Long, verbose style.
  #[default]
  Long,
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
  #[expect(clippy::too_many_lines)]
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let data = self.stuff.stores.get(self.ty);
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
      Data::Array(arr) => {
        let head = if arr.is_set { "set[" } else { "array[" };
        f.write_str(head)?;
        self.with(arr.elem, Prec::Min).fmt(f)?;
        f.write_str("]")
      }
      Data::Tuple(tup) => {
        if tup.elems.is_empty() {
          return f.write_str("unit");
        }
        let [cur_level, new_level] = if let (Some(x), true) =
          (self.stuff.level, tup.elems.len() >= 2 && self.stuff.stores.is_complex(self.ty))
        {
          [Some(x), Some(x + 1)]
        } else {
          [None; 2]
        };
        let mut iter = tup.elems.iter().map(|&ty| self.with(ty, Prec::Min));
        f.write_str("tuple[")?;
        maybe_nl_indent(f, new_level)?;
        if let Some(x) = iter.next() {
          x.fmt(f)?;
        }
        for x in iter {
          f.write_str(",")?;
          field_sep(f, new_level)?;
          x.fmt(f)?;
        }
        maybe_nl_indent(f, cur_level)?;
        f.write_str("]")
      }
      Data::Object(obj) => {
        let [cur_level, new_level] =
          if let (Some(x), true) = (self.stuff.level, self.stuff.stores.is_complex(self.ty)) {
            [Some(x), Some(x + 1)]
          } else {
            [None; 2]
          };
        let mut iter = obj.known.iter().map(|(&key, ty)| FieldDisplay {
          key,
          ty: *ty,
          stuff: Stuff { level: new_level, ..self.stuff },
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
      Data::Fn(func) => match func.parts() {
        Some((params, ret)) => FnDisplay {
          params,
          ret,
          prec: self.prec,
          stuff: Stuff {
            level: if self.stuff.stores.is_complex(self.ty) { self.stuff.level } else { None },
            ..self.stuff
          },
        }
        .fmt(f),
        None => f.write_str("function"),
      },
      Data::Union(tys) => {
        if self.ty == Ty::TOP {
          return f.write_str("top");
        }
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
  params: &'a [Param],
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
    let new_level = self.stuff.level.map(|x| x + 1);
    let mut iter = self
      .params
      .iter()
      .map(|&param| ParamDisplay { param, stuff: Stuff { level: new_level, ..self.stuff } });
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
    if let Some(cur_level) = self.stuff.level {
      f.write_str(",")?;
      nl_indent(f, cur_level)?;
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
  key: jsonnet_expr::Str,
  ty: Ty,
  stuff: Stuff<'a>,
}

impl fmt::Display for FieldDisplay<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let key = self.stuff.str_ar.get(self.key);
    let key_bs = key.as_bytes();
    if jsonnet_ident::is(key_bs) {
      key.fmt(f)?;
    } else {
      jsonnet_escape::Unescape::new(key_bs).fmt(f)?;
    }
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
  stores: Stores<'a>,
  str_ar: &'a StrArena,
  level: Option<usize>,
}

#[derive(Clone, Copy)]
struct Stores<'a> {
  global: &'a GlobalStore,
  local: Option<&'a LocalStore>,
}

impl Stores<'_> {
  fn get(&self, ty: Ty) -> Option<&Data> {
    self.global.0.data(ty, false).or_else(|| self.local?.0.data(ty, true))
  }

  fn complexity(self, ty: Ty) -> u32 {
    let Some(data) = self.get(ty) else { return 0 };
    let inner: u32 = match data {
      Data::Prim(_) => 0,
      Data::Array(arr) => self.complexity(arr.elem),
      Data::Tuple(tup) => tup.elems.iter().map(|&ty| self.complexity(ty)).sum(),
      Data::Object(object) => {
        u32::from(object.has_unknown)
          + object.known.values().map(|&ty| self.complexity(ty)).sum::<u32>()
      }
      Data::Fn(func) => match func.parts() {
        None => 0,
        Some((params, ret)) => params
          .iter()
          .map(|p| p.ty)
          .chain(std::iter::once(ret))
          .map(|ty| self.complexity(ty))
          .sum(),
      },
      Data::Union(parts) => {
        if ty == Ty::BOOLEAN || ty == Ty::TOP {
          0
        } else {
          parts.iter().map(|&ty| self.complexity(ty)).sum()
        }
      }
    };
    1 + inner
  }

  fn is_complex(self, ty: Ty) -> bool {
    self.complexity(ty) >= 6
  }
}

fn field_sep(f: &mut fmt::Formatter<'_>, level: Option<usize>) -> fmt::Result {
  match level {
    None => f.write_str(" "),
    Some(level) => nl_indent(f, level),
  }
}

fn maybe_nl_indent(f: &mut fmt::Formatter<'_>, level: Option<usize>) -> fmt::Result {
  match level {
    None => Ok(()),
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
}
