//! Jsonnet expressions.

#![deny(clippy::pedantic, missing_debug_implementations, rust_2018_idioms)]

mod generated {
  include!(concat!(env!("OUT_DIR"), "/generated.rs"));
}

pub use la_arena::{Arena, ArenaMap, Idx};

use rustc_hash::FxHashMap;
use std::{collections::hash_map::Entry, fmt};

pub type ExprMust = Idx<ExprData>;
pub type Expr = Option<ExprMust>;
pub type ExprArena = Arena<ExprData>;

#[derive(Debug)]
pub enum ExprData {
  Prim(Prim),
  Object {
    asserts: Vec<Expr>,
    fields: Vec<(Expr, Visibility, Expr)>,
  },
  ObjectComp {
    name: Expr,
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
    params: Vec<(Id, Expr)>,
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
    /// TODO make this a Path/PathBuf/PathId/Str?
    path: String,
  },
}

#[derive(Debug, Clone, Copy)]
pub enum ImportKind {
  Code,
  String,
  Binary,
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

/// A primitive value.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Prim {
  Null,
  Bool(bool),
  String(Str),
  Number(Number),
}

impl Prim {
  #[must_use]
  pub fn display<'a>(&'a self, ar: &'a StrArena) -> impl fmt::Display + 'a {
    DisplayPrim { prim: self, ar }
  }
}

struct DisplayPrim<'a> {
  prim: &'a Prim,
  ar: &'a StrArena,
}

impl fmt::Display for DisplayPrim<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self.prim {
      Prim::Null => f.write_str("null"),
      Prim::Bool(b) => b.fmt(f),
      Prim::String(s) => {
        // TODO handle escapes
        f.write_str("\"")?;
        self.ar.get(s).fmt(f)?;
        f.write_str("\"")
      }
      Prim::Number(n) => n.fmt(f),
    }
  }
}
/// A finite floating-point number, that is, one that is not NaN or infinity.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Number(f64);

impl fmt::Display for Number {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    self.0.fmt(f)
  }
}

impl Number {
  /// Returns positive zero.
  #[must_use]
  pub fn positive_zero() -> Self {
    Self(0.0)
  }

  /// Returns positive one.
  #[must_use]
  pub fn positive_one() -> Self {
    Self(1.0)
  }

  /// Returns negative one.
  #[must_use]
  pub fn negative_one() -> Self {
    Self(-1.0)
  }

  /// Exposes the inner value of this number. It will be finite.
  #[must_use]
  pub fn value(&self) -> f64 {
    self.0
  }
}

/// OK because NaN is not allowed
impl Eq for Number {}

impl PartialOrd for Number {
  fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
    Some(self.cmp(other))
  }
}

impl Ord for Number {
  fn cmp(&self, other: &Self) -> std::cmp::Ordering {
    self.value().partial_cmp(&other.value()).expect("should not be NaN")
  }
}

impl TryFrom<f64> for Number {
  type Error = Infinite;

  fn try_from(value: f64) -> Result<Self, Self::Error> {
    if value.is_nan() {
      return Err(Infinite::Nan);
    }
    if value.is_infinite() {
      let inf = if value.is_sign_positive() { Infinite::Pos } else { Infinite::Neg };
      return Err(inf);
    }
    Ok(Self(value))
  }
}

#[derive(Debug)]
pub enum Infinite {
  Nan,
  Pos,
  Neg,
}

impl fmt::Display for Infinite {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Infinite::Nan => f.write_str("not a number"),
      Infinite::Pos => f.write_str("positive infinity"),
      Infinite::Neg => f.write_str("negative infinity"),
    }
  }
}

/// A string, which may be interned.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Str(StrIdx);

/// An interned string, which is an index into a string arena.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct StrIdx(u32);

impl StrIdx {
  /// # Panics
  ///
  /// On failure (i.e. overflow).
  fn from_usize(u: usize) -> Self {
    Self(u.try_into().unwrap())
  }

  /// # Panics
  ///
  /// On failure (i.e. overflow).
  fn to_usize(self) -> usize {
    self.0.try_into().unwrap()
  }
}

#[derive(Debug)]
pub struct StrArena {
  idx_to_contents: Vec<Box<str>>,
  contents_to_idx: FxHashMap<Box<str>, StrIdx>,
}

impl StrArena {
  fn mk_idx(&mut self, contents: Box<str>) -> StrIdx {
    match self.contents_to_idx.entry(contents) {
      Entry::Occupied(entry) => *entry.get(),
      Entry::Vacant(entry) => {
        let ret = StrIdx::from_usize(self.idx_to_contents.len());
        self.idx_to_contents.push(entry.key().clone());
        entry.insert(ret);
        ret
      }
    }
  }

  pub fn str(&mut self, contents: Box<str>) -> Str {
    Str(self.mk_idx(contents))
  }

  pub fn id(&mut self, contents: Box<str>) -> Id {
    Id(self.mk_idx(contents))
  }

  fn get_idx(&self, idx: StrIdx) -> &str {
    &self.idx_to_contents[idx.to_usize()]
  }

  #[must_use]
  pub fn get(&self, s: &Str) -> &str {
    self.get_idx(s.0)
  }
}

/// An identifier.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Id(StrIdx);

impl Id {
  #[must_use]
  pub fn display(self, ar: &StrArena) -> impl fmt::Display + '_ {
    DisplayStrIdx { idx: self.0, ar }
  }
}

struct DisplayStrIdx<'a> {
  idx: StrIdx,
  ar: &'a StrArena,
}

impl fmt::Display for DisplayStrIdx<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    self.ar.get_idx(self.idx).fmt(f)
  }
}

#[derive(Debug, Default)]
pub struct Arenas {
  pub str: StrArena,
  pub expr: ExprArena,
}
