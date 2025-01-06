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
        let old = entry.get();
        let new = old.and(tys, fact);
        entry.insert(new);
      }
      Entry::Vacant(entry) => {
        entry.insert(fact);
      }
    }
  }

  pub(crate) fn get(&self, id: Id) -> Option<&Fact> {
    self.store.get(&id)
  }

  pub(crate) fn remove(&mut self, id: Id) {
    self.store.remove(&id);
  }

  pub(crate) fn negate(&mut self) {
    for f in self.store.values_mut() {
      *f = f.negate();
    }
  }

  pub(crate) fn iter(&self) -> impl Iterator<Item = (&Id, &Fact)> {
    self.store.iter()
  }

  pub(crate) fn into_iter(self) -> impl Iterator<Item = (Id, Fact)> {
    self.store.into_iter()
  }
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct Fact {
  kind: FactKind,
  partial: bool,
}

impl Fact {
  pub(crate) const fn partial(ty: ty::Ty) -> Self {
    Self::with_partiality(ty, true)
  }

  pub(crate) const fn total(ty: ty::Ty) -> Self {
    Self::with_partiality(ty, false)
  }

  pub(crate) const fn with_partiality(ty: ty::Ty, partial: bool) -> Self {
    Self { kind: FactKind::Ty(TyFact::new(ty)), partial }
  }

  pub(crate) fn and(self, tys: &mut ty::MutStore<'_>, other: Self) -> Self {
    let kind = match (self.kind, other.kind) {
      (FactKind::Ty(this), FactKind::Ty(other)) => FactKind::Ty(TyFact {
        is: ty::logic::and(tys, this.is, other.is),
        // de morgan's laws
        is_not: tys.get(ty::Data::mk_union([this.is_not, other.is_not])),
      }),
    };
    Self { kind, partial: self.partial && other.partial }
  }

  pub(crate) fn or(self, tys: &mut ty::MutStore<'_>, other: Self) -> Self {
    let kind = match (self.kind, other.kind) {
      (FactKind::Ty(this), FactKind::Ty(other)) => FactKind::Ty(TyFact {
        is: tys.get(ty::Data::mk_union([this.is, other.is])),
        // de morgan's laws
        is_not: ty::logic::and(tys, this.is_not, other.is_not),
      }),
    };
    Self { kind, partial: self.partial || other.partial }
  }

  pub(crate) fn negate(self) -> Self {
    if self.partial {
      return Self::partial(ty::Ty::ANY);
    }
    let kind = match self.kind {
      FactKind::Ty(this) => FactKind::Ty(TyFact {
        // we don't do self.is = minus(ANY, self.is_not) because we don't support minus(ANY, ...).
        is: ty::Ty::ANY,
        // need to make sure not to minus everything, leaving nothing aka never.
        is_not: if this.is == ty::Ty::ANY { ty::Ty::NEVER } else { this.is },
      }),
    };
    Self { kind, partial: false }
  }

  pub(crate) fn into_ty(self, tys: &mut ty::MutStore<'_>) -> ty::Ty {
    let (is, is_not) = match self.kind {
      FactKind::Ty(this) => (this.is, this.is_not),
    };
    ty::logic::minus(tys, is, is_not)
  }
}

#[derive(Debug, Clone, Copy)]
enum FactKind {
  Ty(TyFact),
}

#[derive(Debug, Clone, Copy)]
struct TyFact {
  is: ty::Ty,
  is_not: ty::Ty,
}

impl TyFact {
  const fn new(is: ty::Ty) -> Self {
    Self { is, is_not: ty::Ty::NEVER }
  }
}
