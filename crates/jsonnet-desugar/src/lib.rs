//! Desugaring and lowering concrete syntax trees into abstract expressions.

#![deny(clippy::pedantic, missing_debug_implementations, rust_2018_idioms)]
#![allow(clippy::needless_pass_by_value, clippy::single_match_else, clippy::too_many_lines)]

mod error;
mod internal;
mod st;

pub use error::Error;
pub use st::Pointers;

#[must_use]
pub fn get(root: jsonnet_syntax::ast::Root) -> Desugar {
  let mut st = st::St::default();
  let top = internal::get_root(&mut st, root);
  let (arenas, pointers, errors) = st.finish();
  Desugar { top, arenas, pointers, errors }
}

#[derive(Debug)]
pub struct Desugar {
  pub top: jsonnet_expr::Expr,
  pub arenas: jsonnet_expr::Arenas,
  pub pointers: Pointers,
  pub errors: Vec<Error>,
}
