//! The data model for facts.

use jsonnet_expr::{Id, Str};
use jsonnet_ty as ty;
use std::collections::{hash_map::Entry, BTreeMap};

#[derive(Debug, Default)]
pub(crate) struct Facts {
  store: rustc_hash::FxHashMap<Id, Fact>,
}

impl Facts {
  pub(crate) fn add(&mut self, id: Id, fact: Fact) {
    match self.store.entry(id) {
      Entry::Occupied(mut entry) => {
        let old = entry.get_mut();
        *old = old.take().and(fact);
      }
      Entry::Vacant(entry) => {
        entry.insert(fact);
      }
    }
  }

  pub(crate) fn remove(&mut self, id: Id) -> Option<Fact> {
    self.store.remove(&id)
  }

  pub(crate) fn negate(&mut self) {
    for f in self.store.values_mut() {
      *f = f.take().not();
    }
  }

  pub(crate) fn iter(&self) -> impl Iterator<Item = (&Id, &Fact)> {
    self.store.iter()
  }

  pub(crate) fn into_iter(self) -> impl Iterator<Item = (Id, Fact)> {
    self.store.into_iter()
  }
}

#[derive(Debug, Clone)]
pub(crate) struct Fact(Repr);

impl Fact {
  pub(crate) fn null() -> Self {
    Self(Prim::Null.into())
  }

  pub(crate) fn true_() -> Self {
    Self(Prim::True.into())
  }

  pub(crate) fn false_() -> Self {
    Self(Prim::False.into())
  }

  pub(crate) fn boolean() -> Self {
    Self(Repr::Or(Box::new(Prim::True.into()), Box::new(Prim::False.into())))
  }

  pub(crate) fn number() -> Self {
    Self(Prim::Number.into())
  }

  pub(crate) fn string() -> Self {
    Self(Prim::String.into())
  }

  pub(crate) fn function() -> Self {
    Self(Prim::Function.into())
  }

  pub(crate) fn array() -> Self {
    Self(Prim::Array.into())
  }

  pub(crate) fn object() -> Self {
    Self(Prim::Object.into())
  }

  pub(crate) fn has_field(field: Str) -> Self {
    Self(Repr::Field(Field { path: vec![field], inner: None }))
  }

  pub(crate) fn for_path(self, path: Vec<Str>) -> Self {
    if path.is_empty() {
      self
    } else {
      Self(Repr::Field(Field { path, inner: Some(Box::new(self.0)) }))
    }
  }

  pub(crate) fn has_len(n: usize) -> Self {
    Self(Repr::Len(n))
  }

  pub(crate) fn and(self, other: Self) -> Self {
    Self(Repr::And(Box::new(self.0), Box::new(other.0)))
  }

  pub(crate) fn or(self, other: Self) -> Self {
    Self(Repr::Or(Box::new(self.0), Box::new(other.0)))
  }

  pub(crate) fn not(self) -> Self {
    Self(Repr::Not(Box::new(self.0)))
  }

  pub(crate) fn apply_to(self, tys: &mut ty::MutStore<'_>, ty: &mut ty::Ty) {
    *ty = self.0.apply_to(tys, *ty);
  }

  /// returns `*self`, putting in its old place a "dummy" fact that should be overwritten later.
  fn take(&mut self) -> Self {
    let mut ret = Self(Repr::Prim(Prim::Null));
    std::mem::swap(self, &mut ret);
    ret
  }
}

#[derive(Debug, Clone)]
enum Repr {
  Prim(Prim),
  Field(Field),
  Len(usize),
  And(Box<Repr>, Box<Repr>),
  Or(Box<Repr>, Box<Repr>),
  Not(Box<Repr>),
}

impl Repr {
  fn apply_to(self, tys: &mut ty::MutStore<'_>, ty: ty::Ty) -> ty::Ty {
    match self {
      Repr::Prim(prim) => ty::logic::and(tys, ty, prim.as_ty()),
      Repr::Field(f) => {
        let obj_ty = f.into_ty(tys);
        ty::logic::and(tys, ty, obj_ty)
      }
      Repr::Len(n) => ty::logic::with_len(tys, ty, n),
      Repr::And(lhs, rhs) => {
        // apply both
        let ty = lhs.apply_to(tys, ty);
        rhs.apply_to(tys, ty)
      }
      Repr::Or(lhs, rhs) => {
        // distribute: a && (b || c) == (a && b) || (a && c)
        let t1 = lhs.apply_to(tys, ty);
        let t2 = rhs.apply_to(tys, ty);
        tys.get(ty::Data::mk_union([t1, t2]))
      }
      Repr::Not(inner) => inner.apply_not(tys, ty),
    }
  }

  fn apply_not(self, tys: &mut ty::MutStore<'_>, ty: ty::Ty) -> ty::Ty {
    match self {
      Repr::Prim(prim) => ty::logic::minus(tys, ty, prim.as_ty()),
      Repr::Field(f) => {
        let obj_ty = f.into_ty(tys);
        ty::logic::minus(tys, ty, obj_ty)
      }
      // ignore
      Repr::Len(_) => ty,
      // de morgan's laws: !(a && b) == !a || !b. see the Or case from apply_to
      Repr::And(lhs, rhs) => {
        let t1 = lhs.apply_not(tys, ty);
        let t2 = rhs.apply_not(tys, ty);
        tys.get(ty::Data::mk_union([t1, t2]))
      }
      // de morgan's laws: !(a || b) == !a && !b. see the And case from apply_to
      Repr::Or(lhs, rhs) => {
        let ty = lhs.apply_not(tys, ty);
        rhs.apply_not(tys, ty)
      }
      // double negative
      Repr::Not(inner) => inner.apply_to(tys, ty),
    }
  }
}

/// all values fall into exactly one prim
#[derive(Debug, Clone)]
enum Prim {
  Null,
  True,
  False,
  Number,
  String,
  Function,
  Array,
  Object,
}

impl Prim {
  fn as_ty(&self) -> ty::Ty {
    match self {
      Prim::Null => ty::Ty::NULL,
      Prim::True => ty::Ty::TRUE,
      Prim::False => ty::Ty::FALSE,
      Prim::Number => ty::Ty::NUMBER,
      Prim::String => ty::Ty::STRING,
      Prim::Function => ty::Ty::UNKNOWN_FN,
      Prim::Array => ty::Ty::ARRAY_ANY,
      Prim::Object => ty::Ty::OBJECT,
    }
  }
}

impl From<Prim> for Repr {
  fn from(value: Prim) -> Self {
    Self::Prim(value)
  }
}

#[derive(Debug, Clone)]
struct Field {
  /// INVARIANT: non-empty.
  path: Vec<Str>,
  /// If this is None, simply asserts the field exists.
  inner: Option<Box<Repr>>,
}

impl Field {
  fn into_ty(self, tys: &mut ty::MutStore<'_>) -> ty::Ty {
    let field_ty = self.inner.map_or(ty::Ty::ANY, |inner| inner.apply_to(tys, ty::Ty::ANY));
    self.path.into_iter().fold(field_ty, |ac, field| {
      tys.get(ty::Data::Object(ty::Object {
        known: BTreeMap::from([(field, ac)]),
        has_unknown: true,
      }))
    })
  }
}
