//! See [`Scope`].

use always::always;
use jsonnet_expr::{def, ExprMust, Id};
use jsonnet_ty as ty;
use rustc_hash::FxHashMap;
use std::collections::hash_map::Entry;

#[derive(Debug)]
struct DefinedId {
  /// INVARIANT: non-empty.
  tys: Vec<ty::Ty>,
  def: def::Def,
  usages: usize,
}

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

  pub(crate) fn into_iter(self) -> impl Iterator<Item = (Id, Fact)> {
    self.store.into_iter()
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
    Self { kind: FactKind::Ty(TyFact { is: ty, is_not: ty::Ty::NEVER }), partial }
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
        // we don't do self.and = minus(ANY, self.minus) because we don't support minus(ANY, ...).
        is: ty::Ty::ANY,
        // need to make sure not to minus everything, leaving nothing aka never.
        is_not: if this.is == ty::Ty::ANY { ty::Ty::NEVER } else { this.is },
      }),
    };
    Self { kind, partial: false }
  }

  /// for when we don't need to do ty logic with the fact, we know it's straight-up the whole truth,
  /// as in [`crate::facts::get_always`].
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

/// Info about the identifiers currently in scope.
#[derive(Debug, Default)]
pub(crate) struct Scope {
  /// This is a vec because things go in and out of scope in stacks.
  store: FxHashMap<Id, Vec<DefinedId>>,
}

impl Scope {
  pub(crate) fn define(&mut self, id: Id, ty: ty::Ty, def: def::Def) {
    self.store.entry(id).or_default().push(DefinedId { tys: vec![ty], def, usages: 0 });
  }

  /// improve the type of an already defined id
  pub(crate) fn refine(&mut self, id: Id, ty: ty::Ty) {
    let Some(defined_id) = self.store.entry(id).or_default().last_mut() else {
      always!(false, "refine without previous define: {id:?}");
      return;
    };
    always!(defined_id.tys.len() == 1, "should only refine at start");
    let Some(t) = defined_id.tys.last_mut() else { return };
    // NOTE: we CANNOT assert *t == ty::Ty::ANY before this, because of duplicate locals like e.g.
    // `local x = 1, x = "hi"; null`
    *t = ty;
  }

  pub(crate) fn get(&mut self, id: Id) -> Option<(ty::Ty, def::Def)> {
    let defined_id = self.store.get_mut(&id)?.last_mut()?;
    defined_id.usages += 1;
    let Some(t) = defined_id.tys.last() else {
      always!(false, "should not have empty ty stack");
      return None;
    };
    Some((*t, defined_id.def))
  }

  /// returns Some(..) if the id was unused
  #[must_use]
  pub(crate) fn undefine(&mut self, id: Id) -> Option<(ExprMust, def::ExprDefKind)> {
    let Some(defined_id) = self.store.entry(id).or_default().pop() else {
      always!(false, "undefine without previous define: {id:?}");
      return None;
    };
    if defined_id.usages != 0 || id == Id::dollar {
      return None;
    }
    let def::Def::Expr(e, k) = defined_id.def else { return None };
    Some((e, k))
  }

  pub(crate) fn add_facts(&mut self, tys: &mut ty::MutStore<'_>, fs: &Facts) {
    for (&id, fact) in &fs.store {
      let Some(stack) = self.store.get_mut(&id) else { continue };
      let Some(defined_id) = stack.last_mut() else { continue };
      let Some(&ty) = defined_id.tys.last() else {
        always!(false, "should not have empty ty stack");
        continue;
      };
      let (is, is_not) = match fact.kind {
        FactKind::Ty(this) => (this.is, this.is_not),
      };
      let ty = ty::logic::and(tys, ty, is);
      let ty = ty::logic::minus(tys, ty, is_not);
      defined_id.tys.push(ty);
    }
  }

  pub(crate) fn remove_facts(&mut self, fs: &Facts) {
    for &id in fs.store.keys() {
      let Some(stack) = self.store.get_mut(&id) else { continue };
      let Some(defined_id) = stack.last_mut() else { continue };
      always!(defined_id.tys.pop().is_some(), "should not have empty ty stack");
    }
  }

  pub(crate) fn is_std(&self, id: Id) -> bool {
    let Some(stack) = self.store.get(&id) else { return false };
    let Some(defined_id) = stack.last() else { return false };
    matches!(defined_id.def, def::Def::Std)
  }

  pub(crate) fn finish(self) {
    for (_, stack) in self.store {
      always!(stack.is_empty());
    }
  }

  pub(crate) fn all_str<'ar>(
    &self,
    str_ar: &'ar jsonnet_expr::StrArena,
  ) -> impl Iterator<Item = &'ar str> + use<'_, 'ar> {
    self.store.iter().map(|(&id, _)| str_ar.get_id(id))
  }
}
