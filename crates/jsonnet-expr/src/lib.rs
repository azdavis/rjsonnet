//! Jsonnet expressions.

#![deny(clippy::pedantic, missing_debug_implementations, rust_2018_idioms)]

mod generated {
  include!(concat!(env!("OUT_DIR"), "/generated.rs"));
}

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
  Mul,
  Div,
  Add,
  Sub,
  Shl,
  Shr,
  Lt,
  LtEq,
  Gt,
  GtEq,
  BitAnd,
  BitXor,
  BitOr,
  LogicalAnd,
  LogicalOr,
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOp {
  Neg,
  Pos,
  LogicalNot,
  BitNot,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Id(Str);

impl Id {
  #[must_use]
  pub fn new(s: Str) -> Self {
    Self(s)
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Str(u32);

impl Str {
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
