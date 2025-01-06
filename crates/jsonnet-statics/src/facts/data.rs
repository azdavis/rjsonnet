//! The data model for facts.

use jsonnet_expr::Id;
use jsonnet_ty as ty;
use std::collections::hash_map::Entry;

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

  pub(crate) const fn ty(ty: ty::Ty) -> Self {
    Self { is: ty, is_not: ty::Ty::NEVER }
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

  pub(crate) fn into_ty(self, tys: &mut ty::MutStore<'_>) -> ty::Ty {
    ty::logic::minus(tys, self.is, self.is_not)
  }
}
