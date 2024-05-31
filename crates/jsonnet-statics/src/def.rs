//! Types related to definition sites.

use jsonnet_expr::Subst;
use rustc_hash::FxHashMap;

/// A definition site for an identifier.
#[derive(Debug, Clone, Copy)]
pub enum Def {
  /// The standard library, `std`.
  Std,
  /// Keyword identifiers, `self` and `super`.
  KwIdent,
  /// An `import` (Jsonnet code only).
  Import(paths::PathId),
  /// A part of an expression.
  Expr(jsonnet_expr::ExprMust, ExprDefKind),
}

/// A definition site with an associated expression.
#[derive(Debug, Clone, Copy)]
pub enum ExprDefKind {
  /// The identifier in an object comprehension.
  ///
  /// ```jsonnet
  /// { [k]: 3 for k in ks }
  /// //           ^ here
  /// ```
  ObjectCompId,
  /// The nth binding in a `local`.
  LocalBind(usize),
  /// The nth function parameter.
  FunctionParam(usize),
}

impl Def {
  /// Apply a subst.
  pub fn apply(&mut self, subst: &Subst) {
    match self {
      Def::Std | Def::KwIdent | Def::Expr(..) => {}
      Def::Import(path_id) => *path_id = subst.get_path_id(*path_id),
    }
  }
}

/// A map from expressions to defs.
pub type Map = FxHashMap<jsonnet_expr::ExprMust, Def>;
