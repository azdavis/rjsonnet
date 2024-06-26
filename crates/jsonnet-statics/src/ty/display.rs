//! Display types.

use super::{Data, Param, Store, Subst, Ty};
use jsonnet_expr::StrArena;
use std::fmt;

impl Ty {
  /// Displays a type.
  ///
  /// Meant to be somewhat similar to how TypeScript does it.
  #[must_use]
  pub fn display<'a>(
    self,
    store: &'a Store,
    subst: &'a Subst,
    str_ar: &'a StrArena,
  ) -> impl fmt::Display + 'a {
    TyDisplay { ty: self, prec: Prec::Min, stuff: Stuff { store, subst, str_ar } }
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
    match self.stuff.store.data(self.stuff.subst, self.ty) {
      Data::Any => f.write_str("any"),
      Data::Bool => f.write_str("boolean"),
      Data::String => f.write_str("string"),
      Data::Number => f.write_str("number"),
      Data::Prim(p) => p.display(self.stuff.str_ar).fmt(f),
      Data::Array(ty) => {
        self.with(ty, Prec::Array).fmt(f)?;
        f.write_str("[]")
      }
      Data::Object(fields) => {
        f.write_str("{")?;
        let mut iter =
          fields.iter().map(|(key, ty)| FieldDisplay { key, ty: *ty, stuff: self.stuff });
        if let Some(field) = iter.next() {
          f.write_str(" ")?;
          field.fmt(f)?;
          for field in iter {
            f.write_str(", ")?;
            field.fmt(f)?;
          }
          f.write_str(" ")?;
        }
        f.write_str("}")
      }
      Data::Fn(func) => {
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
      Data::Meta(_) => f.write_str("_"),
      Data::Union(tys) => {
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
  subst: &'a Subst,
  str_ar: &'a StrArena,
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
