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
  Expr(WithExpr),
}

/// A definition with an expr.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct WithExpr {
  pub expr: ExprMust,
  pub plain: Plain,
  pub sugary: Option<Sugary>,
}

/// A definition site with an associated expression in the plain (not sugary) language.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Plain {
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

/// A place in the sugary syntax that an identifier in definition position may appear.
///
/// This excludes places in the desugared syntax. Those such places are covered by [`Def`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Sugary {
  pub expr: ExprMust,
  pub kind: SugaryKind,
}

/// A kind of sugary def.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SugaryKind {
  /// A local from the nth field in an object.
  ObjectLocal(usize),
  /// The id in an array comprehension.
  ///
  /// ```jsonnet
  /// [x + 1 for x in xs]
  /// //         ^ here
  /// ```
  ArrayComp,
  /// The nth binding from a function defined on a `local` with params.
  LocalFnParam(usize),
  /// The nth binding from a function defined on a field with params.
  FieldFnParam(usize),
  /// A binding from the nth `for` in an object comprehension.
  ObjectComp(usize),
}
