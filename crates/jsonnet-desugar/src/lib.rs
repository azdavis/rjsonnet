//! Desugaring and lowering concrete syntax trees into abstract expressions.

#![deny(clippy::pedantic, missing_debug_implementations, rust_2018_idioms)]
#![allow(clippy::needless_pass_by_value, clippy::single_match_else, clippy::too_many_lines)]

mod internal;
mod st;

#[must_use]
pub fn get(root: jsonnet_syntax::ast::Root) -> Desugar {
  let mut st = st::St::default();
  let top = internal::get_root(&mut st, root);
  let (arenas, errors) = st.finish();
  Desugar { arenas, top, errors }
}

#[derive(Debug)]
pub struct Desugar {
  pub arenas: jsonnet_expr::Arenas,
  pub top: jsonnet_expr::Expr,
  pub errors: Vec<(text_size::TextRange, &'static str)>,
}
