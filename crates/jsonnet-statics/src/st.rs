//! The state of statics.

use crate::{error, ty};
use jsonnet_expr::{def::Def, ExprMust};

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
  /// A store for all the types.
  tys: &'a mut ty::Store,
}

impl<'a> St<'a> {
  /// Make a new state.
  pub fn new(tys: &'a mut ty::Store) -> Self {
    Self { statics: Statics::default(), tys }
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

  pub(crate) fn finish(self) -> Statics {
    self.statics
  }
}
