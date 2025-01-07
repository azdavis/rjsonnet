//! Static checking for jsonnet.

mod check;
mod flow;
mod scope;
mod suggestion;
mod unify;

pub mod error;
pub mod st;

use jsonnet_expr::{Expr, ExprArena, Id};
use jsonnet_ty::{LocalStore, Ty};

/// Performs the checks.
#[must_use]
pub fn get(mut st: st::St<'_>, ar: &ExprArena, expr: Expr) -> (st::Statics, LocalStore) {
  st.scope.define(Id::std, Ty::STD, jsonnet_expr::def::Def::Std);
  st.scope.define(Id::std_unutterable, Ty::STD, jsonnet_expr::def::Def::Std);
  check::get(&mut st, ar, expr);
  st.undefine(Id::std);
  st.undefine(Id::std_unutterable);
  st.finish()
}
