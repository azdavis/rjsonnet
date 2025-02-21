//! The data model for facts.

use jsonnet_expr::{Id, Str};
use jsonnet_ty as ty;
use std::collections::{BTreeMap, hash_map::Entry};

/// A collection of facts about identifiers.
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

/// A fact about an identifier.
#[derive(Debug, Clone)]
pub(crate) struct Fact(Repr);

impl Fact {
  pub(crate) fn null() -> Self {
    Self(Repr::Prim(Prim::Null, Totality::Total))
  }

  pub(crate) fn true_() -> Self {
    Self(Repr::Prim(Prim::True, Totality::Total))
  }

  pub(crate) fn false_() -> Self {
    Self(Repr::Prim(Prim::False, Totality::Total))
  }

  pub(crate) fn boolean() -> Self {
    Self::true_().or(Self::false_())
  }

  pub(crate) fn number(tot: Totality) -> Self {
    Self(Repr::Prim(Prim::Number, tot))
  }

  pub(crate) fn string(tot: Totality) -> Self {
    Self(Repr::Prim(Prim::String, tot))
  }

  pub(crate) fn array(tot: Totality) -> Self {
    Self(Repr::Prim(Prim::Array, tot))
  }

  pub(crate) fn object(tot: Totality) -> Self {
    Self(Repr::Prim(Prim::Object, tot))
  }

  pub(crate) fn function() -> Self {
    Self(Repr::Prim(Prim::Fn, Totality::Total))
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

  pub(crate) fn into_array(self) -> Self {
    Self(Repr::Array(Box::new(self.0)))
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
    let mut ret = Self(Repr::Len(0));
    std::mem::swap(self, &mut ret);
    ret
  }
}

#[derive(Debug, Clone)]
enum Repr {
  Prim(Prim, Totality),
  /// separate from [`Prim::Object`]
  Field(Field),
  Len(usize),
  /// separate from [`Prim::Array`]
  Array(Box<Repr>),
  And(Box<Repr>, Box<Repr>),
  Or(Box<Repr>, Box<Repr>),
  Not(Box<Repr>),
}

impl Repr {
  fn apply_to(self, tys: &mut ty::MutStore<'_>, ty: ty::Ty) -> ty::Ty {
    match self {
      Repr::Prim(prim, _) => ty::logic::and(tys, ty, prim.as_ty()),
      Repr::Field(f) => {
        let field_ty = f.inner.map_or(ty::Ty::ANY, |inner| inner.apply_to(tys, ty::Ty::ANY));
        let obj_ty = obj_with_path_ty(tys, f.path, field_ty);
        ty::logic::and(tys, ty, obj_ty)
      }
      Repr::Len(n) => ty::logic::with_len(tys, ty, n),
      Repr::Array(inner) => {
        let elem = inner.apply_to(tys, ty::Ty::ANY);
        let ary = tys.get(ty::Data::Array(ty::Array::new(elem)));
        ty::logic::and(tys, ty, ary)
      }
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
      Repr::Prim(prim, tot) => match tot {
        Totality::Total => ty::logic::minus(tys, ty, prim.as_ty()),
        // ignore
        Totality::Partial => ty,
      },
      Repr::Field(f) => {
        let field_ty = f.inner.map_or(ty::Ty::NEVER, |inner| inner.apply_not(tys, ty::Ty::TOP));
        let obj_ty = obj_with_path_ty(tys, f.path, field_ty);
        ty::logic::and(tys, ty, obj_ty)
      }
      // ignore:
      // 1. for Len, it's a bit odd to want to assert the length is *not* a certain known number
      // 2. for Array, we have "not all are X", but it does not follow that "all are not X"
      Repr::Len(_) | Repr::Array(_) => ty,
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

/// ALL values fall into EXACTLY ONE of these.
#[derive(Debug, Clone, Copy)]
enum Prim {
  Null,
  True,
  False,
  Number,
  String,
  Array,
  Object,
  Fn,
}

impl Prim {
  fn as_ty(self) -> ty::Ty {
    match self {
      Prim::Null => ty::Ty::NULL,
      Prim::True => ty::Ty::TRUE,
      Prim::False => ty::Ty::FALSE,
      Prim::Number => ty::Ty::NUMBER,
      Prim::String => ty::Ty::STRING,
      Prim::Array => ty::Ty::ARRAY,
      Prim::Object => ty::Ty::OBJECT,
      Prim::Fn => ty::Ty::FUNCTION,
    }
  }
}

#[derive(Debug, Clone)]
struct Field {
  /// INVARIANT: non-empty.
  path: Vec<Str>,
  /// If this is `None`, simply asserts the field exists.
  inner: Option<Box<Repr>>,
}

fn obj_with_path_ty(
  tys: &mut jsonnet_ty::MutStore<'_>,
  path: Vec<Str>,
  ty: jsonnet_ty::Ty,
) -> jsonnet_ty::Ty {
  path.into_iter().fold(ty, |ac, field| {
    let obj = ty::Object { known: BTreeMap::from([(field, ac)]), has_unknown: true };
    tys.get(ty::Data::Object(obj))
  })
}

/// How total a simple fact is.
#[derive(Debug, Clone, Copy)]
pub(crate) enum Totality {
  /// It's total, which means it can be negated.
  ///
  /// For example, if `!std.isNumber(x)` then we KNOW that `x` is not `number`.
  Total,
  /// It's partial, which means it can't be negated.
  ///
  /// For example, if `!std.isInteger(x)` we DO NOT know that `x` is not `number`. It
  /// could be an a decimal number!
  Partial,
}
