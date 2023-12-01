//! Desugaring and lowering concrete syntax trees into abstract expressions.

#![allow(clippy::needless_pass_by_value, clippy::single_match_else, clippy::too_many_lines)]

mod error;
mod escape;
mod internal;
mod st;

pub use error::Error;
pub use st::Pointers;

/// Transforms CST into desugared core.
#[must_use]
pub fn get(root: jsonnet_syntax::ast::Root) -> Desugar {
  let mut st = st::St::default();
  let top = internal::get_root(&mut st, root);
  let (arenas, pointers, errors) = st.finish();
  Desugar { top, arenas, pointers, errors }
}

/// The result of desugaring.
#[derive(Debug)]
pub struct Desugar {
  /// The single top-level expression.
  pub top: jsonnet_expr::Expr,
  /// The arenas holding the allocations.
  pub arenas: jsonnet_expr::Arenas,
  /// Pointers between arena indices and concrete syntax.
  pub pointers: Pointers,
  /// Errors when desugaring.
  pub errors: Vec<Error>,
}
