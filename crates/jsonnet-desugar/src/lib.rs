//! Desugaring and lowering concrete syntax trees into abstract expressions.

#![allow(clippy::needless_pass_by_value, clippy::single_match_else, clippy::too_many_lines)]

mod cx;
mod error;
mod escape;
mod internal;
mod st;

pub use cx::FileSystem;
pub use error::Error;
pub use st::{Desugar, Pointers};

/// Transforms CST into desugared core.
#[must_use]
pub fn get(
  current_dir: &paths::CleanPath,
  other_dirs: &[paths::CleanPathBuf],
  fs: &dyn FileSystem,
  root: Option<jsonnet_syntax::ast::Expr>,
) -> Desugar {
  let mut st = st::St::default();
  let cx = cx::Cx { current_dir, other_dirs, fs };
  let top = internal::get_expr(&mut st, cx, root, false);
  st.finish(top)
}
