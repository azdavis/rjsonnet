//! Jsonnet expressions.

#![deny(clippy::pedantic, missing_debug_implementations, rust_2018_idioms)]

use jsonnet_prim::{Id, Prim, StrArena};
use la_arena::{Arena, Idx};

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

#[derive(Debug, Default)]
pub struct Arenas {
  pub str: StrArena,
  pub expr: ExprArena,
}
