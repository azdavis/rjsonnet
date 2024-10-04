//! Display types.

use super::{Data, Fn, GlobalStore, LocalStore, Param, Prim, StdFnSig, Ty};
use always::always;
use jsonnet_expr::StrArena;
use std::fmt;

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
    TyDisplay {
      ty: self,
      prec: Prec::Min,
      stuff: Stuff {
        global,
        local,
        str_ar,
        level: match multi_line {
          MultiLine::MustNot => None,
          MultiLine::May => Some(0),
        },
      },
    }
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

impl<'a> TyDisplay<'a> {
  fn with(self, ty: Ty, prec: Prec) -> Self {
    Self { ty, prec, ..self }
  }
}

impl<'a> fmt::Display for TyDisplay<'a> {
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
        f.write_str("{")?;
        let [cur_level, new_level, rec_level] = increase_level(obj.known.len(), self.stuff.level);
        let mut iter = obj.known.iter().map(|(key, ty)| FieldDisplay {
          key,
          ty: *ty,
          stuff: Stuff { level: rec_level, ..self.stuff },
        });
        if let Some(field) = iter.next() {
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
        } else if obj.has_unknown {
          f.write_str(" ... ")?;
        }
        f.write_str("}")
      }
      Data::Fn(Fn::Regular(func)) => {
        FnDisplay { params: &func.params, ret: func.ret, prec: self.prec, stuff: self.stuff }.fmt(f)
      }
      Data::Fn(Fn::Std(func)) => match StdFnSig::get(*func) {
        StdFnSig::Simple(params, ret) => {
          FnDisplay { params, ret, prec: self.prec, stuff: self.stuff }.fmt(f)
        }
        StdFnSig::Complex(_) => write!(f, "<std.{func}>"),
      },
      Data::Union(tys) => {
        // special case
        // TODO: make this better: e.g. `true | false | number` should show as `boolean | number`
        if self.ty == Ty::BOOL {
          return f.write_str("boolean");
        }
        let mut iter = tys.iter().map(|&ty| self.with(ty, Prec::Union));
        let Some(ty) = iter.next() else { return f.write_str("never") };
        let needs_paren = self.prec > Prec::Union;
        if needs_paren {
          f.write_str("(")?;
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

impl<'a> fmt::Display for FnDisplay<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let needs_paren = self.prec > Prec::Min;
    if needs_paren {
      f.write_str("(")?;
    }
    f.write_str("(")?;
    let [cur_level, new_level, rec_level] = increase_level(self.params.len(), self.stuff.level);
    let mut iter = self
      .params
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

impl<'a> fmt::Display for FieldDisplay<'a> {
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

impl<'a> fmt::Display for ParamDisplay<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    self.param.id.display(self.stuff.str_ar).fmt(f)?;
    f.write_str(": ")?;
    TyDisplay { ty: self.param.ty, prec: Prec::Min, stuff: self.stuff }.fmt(f)?;
    if !self.param.required {
      f.write_str(" = ...")?;
    }
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

fn nl_indent(f: &mut fmt::Formatter<'_>, level: usize) -> fmt::Result {
  f.write_str("\n")?;
  for _ in 0..level {
    f.write_str("  ")?;
  }
  Ok(())
}

/// Precedence when printing a type.
///
/// One ambiguity is when mixing union types and fn types. We consider 1 and 2 identical in
/// semantics, and distinct from 3. That is, union types "bind closer" than fn types.
///
/// ```text
/// (1) (number) => string | boolean
/// (2) (number) => (string | boolean)
/// (3) ((number) => string) | boolean
/// ```
///
/// Binding closer than that are array types. We consider 1 and 2 identical in semantics, and
/// distinct from 3.
///
/// ```text
/// (1) string | boolean[]
/// (2) string | (boolean[])
/// (3) (string | boolean)[]
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Prec {
  Min,
  Union,
  Array,
}
