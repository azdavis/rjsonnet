//! Jsonnet expressions.

#![allow(missing_docs)]

mod generated {
  include!(concat!(env!("OUT_DIR"), "/generated.rs"));
}

pub mod arg;
pub mod def;
pub mod display;
mod string;

pub use generated::{StdField, StdFn};
pub use string::{Id, Str, StrArena, Subst};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ExprMust(la_arena::Idx<ExprData>);

pub type Expr = Option<ExprMust>;

#[derive(Debug, Default, Clone)]
pub struct ExprArena(la_arena::Arena<ExprData>);

impl std::ops::Index<ExprMust> for ExprArena {
  type Output = ExprData;

  fn index(&self, index: ExprMust) -> &Self::Output {
    &self.0[index.0]
  }
}

impl ExprArena {
  pub fn alloc(&mut self, data: ExprData) -> ExprMust {
    ExprMust(self.0.alloc(data))
  }

  pub fn iter(&self) -> impl Iterator<Item = (ExprMust, &ExprData)> {
    self.0.iter().map(|(a, b)| (ExprMust(a), b))
  }

  pub fn iter_mut(&mut self) -> impl Iterator<Item = (ExprMust, &mut ExprData)> {
    self.0.iter_mut().map(|(a, b)| (ExprMust(a), b))
  }
}

#[derive(Debug, Clone)]
pub struct ExprMap<T>(la_arena::ArenaMap<la_arena::Idx<ExprData>, T>);

impl<T> Default for ExprMap<T> {
  fn default() -> Self {
    Self(la_arena::ArenaMap::default())
  }
}

impl<T> ExprMap<T> {
  pub fn insert(&mut self, key: ExprMust, val: T) {
    self.0.insert(key.0, val);
  }

  #[must_use]
  pub fn get(&self, key: ExprMust) -> Option<&T> {
    self.0.get(key.0)
  }
}

/// Artifacts for combining.
#[derive(Debug, Default)]
pub struct Artifacts {
  /// The paths.
  pub paths: paths::Store,
  /// The strings.
  pub strings: StrArena,
}

#[derive(Debug, Clone)]
pub struct Field {
  pub key: Expr,
  pub plus: bool,
  pub vis: Visibility,
  pub val: Expr,
}

#[derive(Debug, Clone)]
pub enum ExprData {
  Prim(Prim),
  Object {
    asserts: Vec<Expr>,
    fields: Vec<Field>,
  },
  /// object comprehension fields ARE desugared into the body itself, as the spec suggests.
  ObjectComp {
    name: Expr,
    vis: Visibility,
    body: Expr,
    id: Id,
    ary: Expr,
  },
  Array(Vec<Expr>),
  Subscript {
    on: Expr,
    idx: Expr,
  },
  Call {
    func: Expr,
    positional: Vec<Expr>,
    named: Vec<(Id, Expr)>,
  },
  Id(Id),
  Local {
    binds: Vec<(Id, Expr)>,
    body: Expr,
  },
  If {
    cond: Expr,
    yes: Expr,
    no: Expr,
  },
  BinaryOp {
    lhs: Expr,
    op: BinaryOp,
    rhs: Expr,
  },
  UnaryOp {
    op: UnaryOp,
    inner: Expr,
  },
  Function {
    params: Vec<(Id, Option<Expr>)>,
    body: Expr,
  },
  Error(Expr),
  /// contrary to the spec, we do not desugar away imports here. this is because we'd rather not
  /// repeatedly substitute the de-sugared but un-executed jsonnet file contents for every one of
  /// its imports.
  ///
  /// because jsonnet imports are referentially transparent, we can instead evaluate the imported
  /// file to a jsonnet value and cache that instead of the whole expression.
  Import {
    kind: ImportKind,
    path: paths::PathId,
  },
}

impl ExprData {
  pub fn apply(&mut self, subst: &Subst) {
    match self {
      ExprData::Prim(prim) => prim.apply(subst),
      ExprData::ObjectComp { id, .. } | ExprData::Id(id) => id.apply(subst),
      ExprData::Local { binds, .. } | ExprData::Call { named: binds, .. } => {
        for (bind, _) in binds {
          bind.apply(subst);
        }
      }
      ExprData::Function { params, .. } => {
        for (bind, _) in params {
          bind.apply(subst);
        }
      }
      ExprData::Import { path, .. } => *path = subst.get_path_id(*path),
      ExprData::Array(_)
      | ExprData::Object { .. }
      | ExprData::Subscript { .. }
      | ExprData::If { .. }
      | ExprData::BinaryOp { .. }
      | ExprData::UnaryOp { .. }
      | ExprData::Error(_) => {}
    }
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ImportKind {
  Code,
  String,
  Binary,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Visibility {
  Default,
  Hidden,
  Visible,
}

#[derive(Debug, Clone, Copy)]
pub enum BinaryOp {
  Mul,
  Div,
  Add,
  Sub,
  Shl,
  Shr,
  BitAnd,
  BitXor,
  BitOr,
  /// can desugar this to std.equals, but they will share impl anyway.
  Eq,
  Lt,
  LtEq,
  Gt,
  GtEq,
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOp {
  Neg,
  Pos,
  BitNot,
  LogicalNot,
}

/// A primitive value.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Prim {
  Null,
  Bool(bool),
  String(Str),
  Number(finite_float::Float),
}

impl Prim {
  pub fn apply(&mut self, subst: &Subst) {
    match self {
      Prim::Null | Prim::Bool(_) | Prim::Number(_) => {}
      Prim::String(s) => s.apply(subst),
    }
  }
}

#[derive(Debug, Default)]
pub struct Arenas {
  pub str: StrArena,
  pub expr: ExprArena,
}

#[derive(Debug, Default, Clone)]
pub struct Counter {
  inner: ExprMap<usize>,
}

impl Counter {
  pub fn set(&mut self, e: ExprMust, n: usize) {
    self.inner.insert(e, n);
  }

  pub fn is_done(&mut self, e: ExprMust) -> bool {
    let Some(n) = self.inner.0.get_mut(e.0) else { return true };
    if *n == 0 {
      return true;
    }
    *n -= 1;
    *n == 0
  }
}

#[test]
fn size() {
  assert_eq!(std::mem::size_of::<ExprData>(), 56);
}
