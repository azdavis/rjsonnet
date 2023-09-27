//! High-level Intermediate Representation for Jsonnet.

#![deny(clippy::pedantic, missing_debug_implementations, rust_2018_idioms)]

use la_arena::{Arena, Idx};
use rustc_hash::FxHashMap;
use std::collections::hash_map::Entry;

#[derive(Debug, Clone, Copy)]
pub enum Prim {
  Null,
  Bool(bool),
  String(Str),
  Number(f64),
}

pub type ExprMust = Idx<ExprData>;
pub type Expr = Option<ExprMust>;
pub type ExprArena = Arena<ExprData>;

#[derive(Debug)]
pub enum ExprData {
  Prim(Prim),
  Object { asserts: Vec<Expr>, fields: Vec<(Expr, Visibility, Expr)> },
  ObjectComp { name: Expr, body: Expr, id: Id, ary: Expr },
  Array(Vec<Expr>),
  Subscript { on: Expr, idx: Expr },
  Call { func: Expr, positional: Vec<Expr>, named: Vec<(Id, Expr)> },
  Id(Id),
  Local { binds: Vec<(Id, Expr)>, body: Expr },
  If { cond: Expr, yes: Expr, no: Expr },
  BinaryOp { lhs: Expr, op: BinaryOp, rhs: Expr },
  UnaryOp { op: UnaryOp, inner: Expr },
  Function { params: Vec<(Id, Expr)>, body: Expr },
  Error(Expr),
}

#[derive(Debug, Clone, Copy)]
pub enum Visibility {
  Default,
  Hidden,
  Visible,
}

#[derive(Debug, Clone, Copy)]
pub enum BinaryOp {
  Star,
  Slash,
  Plus,
  Minus,
  LtLt,
  GtGt,
  Lt,
  LtEq,
  Gt,
  GtEq,
  And,
  Carat,
  Bar,
  AndAnd,
  BarBar,
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOp {
  Minus,
  Plus,
  Bang,
  Tilde,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Id(Str);

impl Id {
  pub const STD: Self = Self(Str::STD);
  pub const SELF: Self = Self(Str::SELF);
  pub const SUPER: Self = Self(Str::SUPER);
  pub const DOLLAR: Self = Self(Str::DOLLAR);

  #[must_use]
  pub fn new(s: Str) -> Self {
    Self(s)
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Str(u32);

impl Str {
  pub const STD: Self = Self(0);
  pub const SELF: Self = Self(1);
  pub const SUPER: Self = Self(2);
  pub const DOLLAR: Self = Self(3);
  pub const PARAMETER_NOT_BOUND: Self = Self(4);
  pub const TODO: Self = Self(5);

  const PRESET: [(Self, &'static str); 6] = [
    (Self::STD, "std"),
    (Self::SELF, "self"),
    (Self::SUPER, "super"),
    (Self::DOLLAR, "$"),
    (Self::PARAMETER_NOT_BOUND, "Parameter not bound"),
    (Self::TODO, "TODO"),
  ];

  /// Panics on failure.
  fn from_usize(u: usize) -> Self {
    Self(u.try_into().unwrap())
  }

  /// Panics on failure.
  fn to_usize(self) -> usize {
    self.0.try_into().unwrap()
  }
}

#[derive(Debug)]
pub struct StrArena {
  id_to_contents: Vec<Box<str>>,
  contents_to_id: FxHashMap<Box<str>, Str>,
}

impl Default for StrArena {
  fn default() -> Self {
    let mut ret = Self { id_to_contents: Vec::new(), contents_to_id: FxHashMap::default() };
    for (a, b) in Str::PRESET {
      assert_eq!(a, ret.insert(b.to_owned().into_boxed_str()));
    }
    ret
  }
}

impl StrArena {
  pub fn insert(&mut self, contents: Box<str>) -> Str {
    match self.contents_to_id.entry(contents) {
      Entry::Occupied(entry) => *entry.get(),
      Entry::Vacant(entry) => {
        let ret = Str::from_usize(self.id_to_contents.len());
        self.id_to_contents.push(entry.key().clone());
        entry.insert(ret);
        ret
      }
    }
  }

  #[must_use]
  pub fn get(&self, s: Str) -> &str {
    &self.id_to_contents[s.to_usize()]
  }
}

#[derive(Debug, Default)]
pub struct Arenas {
  pub str: StrArena,
  pub expr: ExprArena,
}
