//! Static checking for jsonnet.

mod check;
mod unify;

pub mod error;
pub mod st;
pub mod ty;

use jsonnet_expr::{Arenas, Expr, Id};

/// Performs the checks.
#[must_use]
pub fn get(mut st: st::St<'_>, ars: &Arenas, expr: Expr) -> (st::Statics, ty::LocalStore) {
  st.define(Id::std, ty::Ty::ANY, jsonnet_expr::def::Def::Std);
  st.define(Id::std_unutterable, ty::Ty::ANY, jsonnet_expr::def::Def::Std);
  check::get(&mut st, ars, expr);
  st.undefine(Id::std);
  st.undefine(Id::std_unutterable);
  st.finish()
}
