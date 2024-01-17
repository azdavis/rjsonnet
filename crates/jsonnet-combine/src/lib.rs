//! Combine some artifacts.

/// Artifacts for combining.
#[derive(Debug, Default)]
pub struct Artifacts {
  /// The paths.
  pub paths: paths::Store,
  /// The strings.
  pub strings: jsonnet_expr::StrArena,
}

/// Combine.
pub fn get(
  art: &mut Artifacts,
  other: Artifacts,
  _: &mut jsonnet_expr::ExprArena,
  _: jsonnet_expr::Expr,
) {
  // TODO
  *art = other;
}
