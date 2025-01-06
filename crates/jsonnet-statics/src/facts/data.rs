//! The data model for facts.

use always::always;
use jsonnet_expr::{Id, Str};
use jsonnet_ty as ty;
use std::collections::{hash_map::Entry, BTreeMap};

#[derive(Debug, Default)]
pub(crate) struct Facts {
  store: rustc_hash::FxHashMap<Id, Fact>,
}

impl Facts {
  pub(crate) fn add(&mut self, tys: &mut ty::MutStore<'_>, id: Id, fact: Fact) {
    match self.store.entry(id) {
      Entry::Occupied(mut entry) => {
        let old = entry.get_mut();
        *old = old.take().and(tys, fact);
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
pub(crate) struct Fact {
  is: ty::Ty,
  is_not: ty::Ty,
}

#[expect(clippy::needless_pass_by_value)]
impl Fact {
  /// returns `*self`, putting in its old place a "dummy" fact that should be overwritten later.
  fn take(&mut self) -> Self {
    let mut ret = Self { is: ty::Ty::ANY, is_not: ty::Ty::ANY };
    std::mem::swap(self, &mut ret);
    ret
  }

  const fn ty(ty: ty::Ty) -> Self {
    Self { is: ty, is_not: ty::Ty::NEVER }
  }

  pub(crate) fn null() -> Self {
    Self::ty(ty::Ty::NULL)
  }

  pub(crate) fn true_() -> Self {
    Self::ty(ty::Ty::TRUE)
  }

  pub(crate) fn false_() -> Self {
    Self::ty(ty::Ty::FALSE)
  }

  pub(crate) fn array() -> Self {
    Self::ty(ty::Ty::ARRAY_ANY)
  }

  pub(crate) fn boolean() -> Self {
    Self::ty(ty::Ty::BOOL)
  }

  pub(crate) fn number() -> Self {
    Self::ty(ty::Ty::NUMBER)
  }

  pub(crate) fn object() -> Self {
    Self::ty(ty::Ty::OBJECT)
  }

  pub(crate) fn string() -> Self {
    Self::ty(ty::Ty::STRING)
  }

  pub(crate) fn function() -> Self {
    Self::ty(ty::Ty::UNKNOWN_FN)
  }

  pub(crate) fn has_field(tys: &mut ty::MutStore<'_>, field: Str) -> Self {
    let ty = tys.get(ty::Data::Object(ty::Object {
      known: BTreeMap::from([(field, ty::Ty::ANY)]),
      has_unknown: true,
    }));
    Self::ty(ty)
  }

  pub(crate) fn for_path(mut self, tys: &mut ty::MutStore<'_>, path: Vec<Str>) -> Self {
    always!(self.is_not == ty::Ty::NEVER);
    for field in path {
      self.is = tys.get(ty::Data::Object(ty::Object {
        known: BTreeMap::from([(field, self.is)]),
        has_unknown: true,
      }));
    }
    self
  }

  pub(crate) fn and(self, tys: &mut ty::MutStore<'_>, other: Self) -> Self {
    Self {
      is: ty::logic::and(tys, self.is, other.is),
      // de morgan's laws
      is_not: tys.get(ty::Data::mk_union([self.is_not, other.is_not])),
    }
  }

  pub(crate) fn or(self, tys: &mut ty::MutStore<'_>, other: Self) -> Self {
    Self {
      is: tys.get(ty::Data::mk_union([self.is, other.is])),
      // de morgan's laws
      is_not: ty::logic::and(tys, self.is_not, other.is_not),
    }
  }

  pub(crate) fn not(self) -> Self {
    Self {
      // we don't do self.is = minus(ANY, self.is_not) because we don't support minus(ANY, ...).
      is: ty::Ty::ANY,
      // need to make sure not to minus everything, leaving nothing aka never.
      is_not: if self.is == ty::Ty::ANY { ty::Ty::NEVER } else { self.is },
    }
  }

  pub(crate) fn apply_to(&self, tys: &mut ty::MutStore<'_>, ty: &mut ty::Ty) {
    *ty = ty::logic::and(tys, *ty, self.is);
    *ty = ty::logic::minus(tys, *ty, self.is_not);
  }
}
