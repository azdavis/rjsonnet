//! The state of statics.

use crate::{error, ty, unify};
use always::always;
use jsonnet_expr::{def::Def, ExprMust, Id};
use rustc_hash::FxHashMap;

/// Results after doing statics on one file.
#[derive(Debug, Default)]
pub struct Statics {
  /// The errors.
  pub errors: Vec<error::Error>,
  /// Any definition sites we could figure out.
  pub defs: jsonnet_expr::def::Map,
  /// Types of expressions.
  pub expr_tys: ty::Exprs,
}

#[derive(Debug)]
struct DefinedId {
  ty: ty::Ty,
  def: Def,
  usages: usize,
}

/// The state when checking statics.
#[derive(Debug)]
pub struct St<'a> {
  /// The in-progress results for this file.
  statics: Statics,
  /// Stores the identifiers currently in scope.
  ///
  /// This is a vec because things go in and out of scope in stacks.
  context: FxHashMap<Id, Vec<DefinedId>>,
  /// A generator for meta variables.
  #[allow(dead_code)]
  meta_gen: ty::MetaGen,
  /// A store for all the types.
  tys: &'a mut ty::Store,
  /// A subst for all the meta vars.
  subst: &'a mut ty::Subst,
}

impl<'a> St<'a> {
  /// Make a new state.
  pub fn new(tys: &'a mut ty::Store, subst: &'a mut ty::Subst) -> Self {
    Self {
      statics: Statics::default(),
      context: FxHashMap::default(),
      meta_gen: ty::MetaGen::default(),
      tys,
      subst,
    }
  }

  pub(crate) fn err(&mut self, expr: ExprMust, kind: error::Kind) {
    self.statics.errors.push(error::Error { expr, kind });
  }

  pub(crate) fn note_usage(&mut self, expr: ExprMust, def: Def) {
    // NOTE: we CANNOT assert insert returns none here, because we reuse expr indices sometimes
    // when desugaring.
    self.statics.defs.insert(expr, def);
  }

  pub(crate) fn get_ty(&mut self, data: ty::Data) -> ty::Ty {
    self.tys.get(data)
  }

  pub(crate) fn insert_expr_ty(&mut self, expr: ExprMust, ty: ty::Ty) {
    self.statics.expr_tys.insert(expr, ty);
  }

  pub(crate) fn define(&mut self, id: Id, ty: ty::Ty, def: Def) {
    self.context.entry(id).or_default().push(DefinedId { ty, def, usages: 0 });
  }

  pub(crate) fn undefine(&mut self, id: Id) {
    let Some(in_scope) = self.context.entry(id).or_default().pop() else {
      always!(false, "undefine without previous define: {id:?}");
      return;
    };
    if in_scope.usages != 0 || id == Id::dollar {
      return;
    }
    let Def::Expr(e, k) = in_scope.def else { return };
    self.err(e, error::Kind::Unused(id, k));
  }

  pub(crate) fn define_self_super(&mut self) {
    self.define(Id::self_, ty::Ty::ANY, Def::KwIdent);
    self.define(Id::super_, ty::Ty::ANY, Def::KwIdent);
  }

  pub(crate) fn undefine_self_super(&mut self) {
    self.undefine(Id::self_);
    self.undefine(Id::super_);
  }

  pub(crate) fn get(&mut self, id: Id) -> Option<(ty::Ty, Def)> {
    let in_scope = self.context.get_mut(&id)?.last_mut()?;
    in_scope.usages += 1;
    Some((in_scope.ty, in_scope.def))
  }

  /// TODO remove
  #[allow(dead_code)]
  pub(crate) fn unify(&mut self, expr: ExprMust, want: ty::Ty, got: ty::Ty) {
    let mut st = unify::St { expr, errors: &mut self.statics.errors, subst: self.subst };
    unify::get(&mut st, self.tys, want, got);
  }

  #[must_use]
  #[allow(dead_code)]
  pub(crate) fn fresh(&mut self) -> ty::Ty {
    let data = ty::Data::Meta(self.meta_gen.gen());
    self.get_ty(data)
  }

  pub(crate) fn finish(self) -> Statics {
    for (_, stack) in self.context {
      always!(stack.is_empty());
    }
    self.statics
  }
}
