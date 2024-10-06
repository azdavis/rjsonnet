//! Types related to definition sites.

use crate::ExprMust;
use rustc_hash::FxHashMap;

/// A definition site for an identifier without an expr.
///
/// NOTE: no need to apply an expr subst because we run statics after combining the per-file
/// syntax artifacts.
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
  /// The nth binding in a multi-def site.
  Multi(usize, ExprDefKindMulti),
}

/// A definition site from one of the things that can introduce multiple bindings at once.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ExprDefKindMulti {
  /// A binding in a `local`.
  LocalBind,
  /// A binding in a `local` inside of an object.
  ObjectLocalBind,
  /// A parameter in a function.
  FnParam,
}

/// A map from expressions to defs.
pub type Map = FxHashMap<ExprMust, Def>;
