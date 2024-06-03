//! Types related to definition sites.

use crate::{subst::Subst, ExprMust};
use rustc_hash::FxHashMap;

/// A definition site for an identifier.
#[derive(Debug, Clone, Copy)]
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

/// A definition site with an associated expression.
#[derive(Debug, Clone, Copy)]
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

/// A place in the sugary syntax that an expr may appear.
///
/// This excludes places in the desugared syntax. Those such places are covered by
/// [`crate::def::Def`].
#[derive(Debug, Clone, Copy)]
pub enum Sugary {
  /// A local from the nth field in an object.
  ObjectLocal(usize),
  /// The id in an array comprehension.
  ///
  /// ```jsonnet
  /// [x + 1 for x in xs]
  /// //         ^ here
  /// ```
  ArrayComp,
  /// The nth binding from a function defined on a `local` with params,
  LocalFnParam(usize),
  /// The nth binding from a function defined on a field with params,
  FieldFnParam(usize),
  /// A binding from the nth `for` in an object comprehension.
  ObjectComp(usize),
}

impl Sugary {
  #[must_use]
  pub fn local_fn_param(n: usize) -> Option<Self> {
    Some(Self::LocalFnParam(n))
  }

  #[must_use]
  pub fn field_fn_param(n: usize) -> Option<Self> {
    Some(Self::FieldFnParam(n))
  }
}
