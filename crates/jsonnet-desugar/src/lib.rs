//! Desugaring and lowering CST into HIR.

#![deny(clippy::pedantic, missing_debug_implementations, rust_2018_idioms)]
#![allow(clippy::needless_pass_by_value)]

mod expr;
mod st;

use jsonnet_hir::Arenas;

#[must_use]
pub fn get(root: jsonnet_syntax::ast::Root) -> Desugar {
  let mut st = st::St::default();
  let top = expr::expr(&mut st, root.expr());
  Desugar { arenas: st.finish(), top }
}

#[derive(Debug)]
pub struct Desugar {
  pub arenas: Arenas,
  pub top: jsonnet_hir::Expr,
}
