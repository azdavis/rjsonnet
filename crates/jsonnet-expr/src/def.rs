//! Types related to definition sites.

use crate::{subst::Subst, ExprMust};
use rustc_hash::FxHashMap;

/// A definition site for an identifier without an expr.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Def {
  /// The standard library, `std`.
  Std,
  /// Keyword identifiers, `self` and `super`.
  KwIdent,
  /// An `import` of a Jsonnet file.
  Import(paths::PathId),
  /// A part of an expression.
  Expr(ExprMust, ExprDefKind),
}

/// A definition site with an associated expression in the plain (not sugary) language.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ExprDefKind {
  /// The identifier in an object comprehension.
  ///
  /// ```jsonnet
  /// { [f(k)]: g(k) for k in ks }
  /// //                 ^ here
  /// ```
  ObjectCompId,
  /// The nth binding in a `local`.
  LocalBind(usize),
  /// The nth function parameter.
  FnParam(usize),
  /// An object local.
  ObjectLocal(usize),
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
pub type Map = FxHashMap<ExprMust, Def>;
