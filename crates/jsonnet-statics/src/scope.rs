//! See [`Scope`].

use always::always;
use jsonnet_expr::{def, ExprMust, Id};
use jsonnet_ty as ty;
use rustc_hash::FxHashMap;

#[derive(Debug)]
struct DefinedId {
  /// INVARIANT: non-empty.
  tys: Vec<ty::Ty>,
  def: def::Def,
  usages: usize,
}

pub(crate) type Facts = rustc_hash::FxHashMap<Id, Fact>;

#[derive(Debug, Clone, Copy)]
pub(crate) struct Fact {
  ty: ty::Ty,
  partial: bool,
}

impl Fact {
  pub(crate) const fn partial(ty: ty::Ty) -> Self {
    Self { ty, partial: true }
  }

  pub(crate) const fn total(ty: ty::Ty) -> Self {
    Self { ty, partial: false }
  }

  pub(crate) fn and(self, tys: &mut ty::MutStore<'_>, other: Self) -> Self {
    Self { ty: ty::logic::and(tys, self.ty, other.ty), partial: self.partial && other.partial }
  }

  pub(crate) fn or(self, tys: &mut ty::MutStore<'_>, other: Self) -> Self {
    Self {
      ty: tys.get(ty::Data::mk_union([self.ty, other.ty])),
      partial: self.partial || other.partial,
    }
  }

  /// for when we don't need to do ty logic with the fact, we know it's straight-up the whole truth,
  /// as in [`crate::facts::get_always`].
  pub(crate) fn into_ty(self) -> ty::Ty {
    self.ty
  }
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
    for (&id, fact) in fs {
      let Some(stack) = self.store.get_mut(&id) else { continue };
      let Some(defined_id) = stack.last_mut() else { continue };
      let Some(&cur) = defined_id.tys.last() else {
        always!(false, "should not have empty ty stack");
        continue;
      };
      let new = ty::logic::and(tys, cur, fact.ty);
      defined_id.tys.push(new);
    }
  }

  pub(crate) fn negate_facts(&mut self, tys: &mut ty::MutStore<'_>, fs: &Facts) {
    for (&id, fact) in fs {
      let Some(stack) = self.store.get_mut(&id) else { continue };
      let Some(defined_id) = stack.last_mut() else { continue };
      let Some(&cur) = defined_id.tys.last() else {
        always!(false, "should not have empty ty stack");
        continue;
      };
      if fact.partial {
        // just push cur again so that we always have something to pop in remove
        defined_id.tys.push(cur);
      } else {
        let new = ty::logic::minus(tys, cur, fact.ty);
        defined_id.tys.push(new);
      }
    }
  }

  pub(crate) fn remove_facts(&mut self, fs: &Facts) {
    for &id in fs.keys() {
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
}
