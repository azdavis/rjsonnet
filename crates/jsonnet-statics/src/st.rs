//! The state of statics.

use crate::{error, scope::Scope, unify};
use jsonnet_expr::{def, ExprMust, Id};
use jsonnet_ty as ty;
use paths::PathMap;
use rustc_hash::FxHashSet;

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

/// The state when checking statics.
#[derive(Debug)]
pub struct St<'a> {
  /// The in-progress results for this file.
  statics: Statics,
  /// The types of other files.
  other_files: &'a PathMap<ty::Ty>,
  /// The strings.
  pub(crate) str_ar: &'a jsonnet_expr::StrArena,
  /// The things in scope.
  pub(crate) scope: Scope,
  /// A store for all the types.
  pub(crate) tys: ty::MutStore<'a>,
  /// The object comp local defs we've seen before.
  pub(crate) object_comp_local_defs: FxHashSet<ExprMust>,
}

impl<'a> St<'a> {
  /// Make a new state.
  #[must_use]
  pub fn new(
    tys: &'a ty::GlobalStore,
    other_files: &'a PathMap<ty::Ty>,
    str_ar: &'a jsonnet_expr::StrArena,
  ) -> Self {
    Self {
      statics: Statics::default(),
      other_files,
      str_ar,
      scope: Scope::default(),
      tys: ty::MutStore::new(tys),
      object_comp_local_defs: FxHashSet::default(),
    }
  }

  pub(crate) fn err(&mut self, expr: ExprMust, kind: error::Kind) {
    self.statics.errors.push(error::Error { expr, kind });
  }

  pub(crate) fn note_usage(&mut self, expr: ExprMust, def: def::Def) {
    // NOTE: we CANNOT assert insert returns None here, because we reuse expr indices sometimes
    // when desugaring.
    self.statics.defs.insert(expr, def);
  }

  pub(crate) fn define_self_super(&mut self) {
    self.scope.define(Id::self_, ty::Ty::OBJECT, def::Def::KwIdent);
    self.scope.define(Id::super_, ty::Ty::OBJECT, def::Def::KwIdent);
  }

  pub(crate) fn undefine_self_super(&mut self) {
    // these can never be marked as unused
    _ = self.scope.undefine(Id::self_);
    _ = self.scope.undefine(Id::super_);
  }

  pub(crate) fn insert_expr_ty(&mut self, expr: ExprMust, ty: ty::Ty) {
    self.statics.expr_tys.insert(expr, ty);
  }

  pub(crate) fn unify(&mut self, expr: ExprMust, want: ty::Ty, got: ty::Ty) {
    let mut st = unify::St::new(self.str_ar);
    unify::get(&mut st, &self.tys, want, got);
    for u in st.finish() {
      self.err(expr, error::Kind::Unify(u));
    }
  }

  pub(crate) fn finish(self) -> (Statics, ty::LocalStore) {
    self.scope.finish();
    (self.statics, self.tys.into_local())
  }

  pub(crate) fn import_ty(&self, path: paths::PathId) -> ty::Ty {
    if let Some(&x) = self.other_files.get(&path) {
      x
    } else {
      log::warn!("import_ty failed: {path:?}");
      ty::Ty::ANY
    }
  }
}
