//! Display types.

use super::{Data, Fn, GlobalStore, Param, Prim, Store, Ty};
use always::always;
use jsonnet_expr::StrArena;
use std::fmt;

impl Ty {
  /// Displays a type.
  ///
  /// Meant to be somewhat similar to how TypeScript does it.
  #[must_use]
  pub fn display<'a>(
    self,
    multi_line: MultiLine,
    store: &'a GlobalStore,
    str_ar: &'a StrArena,
  ) -> impl fmt::Display + 'a {
    TyDisplay {
      ty: self,
      prec: Prec::Min,
      stuff: Stuff {
        store: &store.0,
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
    let Some(data) = self.stuff.store.data(self.ty, false) else {
      always!(false, "should not use local store for display");
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
        let new_level = self.stuff.level.map(|x| x + 1);
        let mut iter = obj.known.iter().map(|(key, ty)| FieldDisplay {
          key,
          ty: *ty,
          stuff: Stuff { level: new_level, ..self.stuff },
        });
        if let Some(field) = iter.next() {
          let (level, new_level) =
            if obj.known.len() < 4 { (None, None) } else { (self.stuff.level, new_level) };
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
          field_sep(f, level)?;
        } else if obj.has_unknown {
          f.write_str(" ... ")?;
        }
        f.write_str("}")
      }
      Data::Fn(Fn::Regular(func)) => {
        let needs_paren = self.prec > Prec::Min;
        if needs_paren {
          f.write_str("(")?;
        }
        f.write_str("(")?;
        let mut iter = func.params.iter().map(|&param| ParamDisplay { param, stuff: self.stuff });
        if let Some(ty) = iter.next() {
          ty.fmt(f)?;
        }
        for ty in iter {
          f.write_str(", ")?;
          ty.fmt(f)?;
        }
        f.write_str(") => ")?;
        self.with(func.ret, self.prec).fmt(f)?;
        if needs_paren {
          f.write_str(")")?;
        }
        Ok(())
      }
      Data::Fn(Fn::Std(func)) => write!(f, "<std.{func}>"),
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
    if !self.param.required {
      f.write_str("?")?;
    }
    f.write_str(": ")?;
    TyDisplay { ty: self.param.ty, prec: Prec::Min, stuff: self.stuff }.fmt(f)
  }
}

/// Just a bunch of common stuff we need in a few places. Naming is hard.
#[derive(Clone, Copy)]
struct Stuff<'a> {
  store: &'a Store,
  str_ar: &'a StrArena,
  level: Option<usize>,
}

fn field_sep(f: &mut fmt::Formatter<'_>, level: Option<usize>) -> fmt::Result {
  match level {
    None => f.write_str(" "),
    Some(level) => {
      f.write_str("\n")?;
      for _ in 0..level {
        f.write_str("  ")?;
      }
      Ok(())
    }
  }
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
