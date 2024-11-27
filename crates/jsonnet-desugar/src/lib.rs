//! Desugaring and lowering concrete syntax trees into abstract expressions.

#![expect(clippy::needless_pass_by_value, clippy::single_match_else, clippy::too_many_lines)]

mod cx;
mod error;
mod internal;
mod st;

pub use error::Error;
pub use st::{Desugar, Pointers};

/// Transforms CST into desugared core.
#[must_use]
pub fn get(
  dirs: jsonnet_resolve_import::NonEmptyDirs<'_>,
  fs: &dyn jsonnet_resolve_import::FileSystem,
  root: Option<jsonnet_syntax::ast::Expr>,
) -> Desugar {
  let mut st = st::St::default();
  let cx = cx::Cx { dirs, fs };
  let top = internal::get_expr(&mut st, cx, root, false, false);
  st.finish(top)
}
