//! Static checking for jsonnet.

mod check;
mod flow;
mod scope;
mod suggestion;
mod unify;

pub mod error;
pub mod st;

use jsonnet_expr::{Expr, ExprArena, Id};
use jsonnet_ty::Ty;

/// Performs the checks.
#[must_use]
pub fn get(mut st: st::St<'_>, ar: &ExprArena, expr: Expr) -> st::Finish {
  st.scope.define(Id::std, Ty::STD, jsonnet_expr::def::Def::Std);
  st.scope.define(Id::std_unutterable, Ty::STD, jsonnet_expr::def::Def::Std);
  check::get(&mut st, ar, expr);
  // these can never be marked as unused
  _ = st.scope.undefine(Id::std);
  _ = st.scope.undefine(Id::std_unutterable);
  st.finish()
}
