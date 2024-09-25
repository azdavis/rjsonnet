//! Jsonnet expressions.

#![allow(missing_docs)]

mod generated {
  include!(concat!(env!("OUT_DIR"), "/generated.rs"));
}

pub mod arg;
pub mod def;
pub mod display;
mod subst;

pub use generated::{std_fn, StdFn};
pub use la_arena::{Arena, ArenaMap, Idx};
pub use subst::Subst;

use generated::{BuiltinStr, NotBuiltinStr};
use rustc_hash::FxHashMap;
use std::collections::hash_map::Entry;

pub type ExprMust = Idx<ExprData>;
pub type Expr = Option<ExprMust>;
pub type ExprArena = Arena<ExprData>;

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
  /// object fields ARE NOT desugared into the body itself. evaluation is thus modified to handle
  /// this new location that ids in def position are permitted to appear.
  ///
  /// the spec suggests duplicating the locals across every assert and field, but that is a bit
  /// wasteful and also makes implementing unused variables prohibitively tricky.
  Object {
    binds: Vec<(Id, Expr)>,
    asserts: Vec<Expr>,
    fields: Vec<Field>,
  },
  /// object comprehension fields ARE desugared into the body itself, as the spec suggests.
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

const _: () = assert!(std::mem::size_of::<ExprData>() == 80);

impl ExprData {
  pub fn apply(&mut self, subst: &Subst) {
    match self {
      ExprData::Prim(prim) => prim.apply(subst),
      ExprData::ObjectComp { id, .. } | ExprData::Id(id) => id.apply(subst),
      ExprData::Object { binds, fields: _, asserts: _ }
      | ExprData::Local { binds, .. }
      | ExprData::Call { named: binds, .. } => {
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

/// A string, which may be interned.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Str(StrRepr);

impl Str {
  pub fn apply(&mut self, subst: &Subst) {
    match &mut self.0 {
      StrRepr::Copy(repr) => repr.apply(subst),
      StrRepr::Alloc(_) => {}
    }
  }

  pub(crate) const fn builtin(bs: BuiltinStr) -> Self {
    Self(StrRepr::Copy(CopyStrRepr::Builtin(bs)))
  }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum StrRepr {
  Copy(CopyStrRepr),
  Alloc(Box<str>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum CopyStrRepr {
  Builtin(BuiltinStr),
  Idx(StrIdx),
}

impl CopyStrRepr {
  fn apply(&mut self, subst: &Subst) {
    match self {
      CopyStrRepr::Builtin(_) => {}
      CopyStrRepr::Idx(idx) => *idx = subst.get_str_idx(*idx),
    }
  }
}

/// An interned string, which is an index into a string arena.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct StrIdx(u32);

impl StrIdx {
  fn from_usize(n: usize) -> Self {
    Self(always::convert::usize_to_u32(n))
  }

  fn to_usize(self) -> usize {
    always::convert::u32_to_usize(self.0)
  }
}

#[derive(Debug, Default)]
pub struct StrArena {
  idx_to_data: Vec<Box<str>>,
  data_to_idx: FxHashMap<Box<str>, StrIdx>,
}

impl StrArena {
  /// Should only call this when we know the contents are NOT one of the builtin strings. The
  /// `NotBuiltinStr` argument serves as a witness to this fact.
  fn dangerous_mk_idx(&mut self, contents: Box<str>, _: NotBuiltinStr) -> StrIdx {
    match self.data_to_idx.entry(contents) {
      Entry::Occupied(entry) => *entry.get(),
      Entry::Vacant(entry) => {
        let ret = StrIdx::from_usize(self.idx_to_data.len());
        self.idx_to_data.push(entry.key().clone());
        entry.insert(ret);
        ret
      }
    }
  }

  fn mk_copy_repr(&mut self, contents: Box<str>) -> CopyStrRepr {
    match contents.as_ref().parse::<BuiltinStr>() {
      Ok(bs) => CopyStrRepr::Builtin(bs),
      Err(nbs) => CopyStrRepr::Idx(self.dangerous_mk_idx(contents, nbs)),
    }
  }

  /// inserts the contents if it was not in the arena already
  pub fn str(&mut self, contents: Box<str>) -> Str {
    Str(StrRepr::Copy(self.mk_copy_repr(contents)))
  }

  /// uses the contents if it was in the arena already, else does NOT insert
  #[must_use]
  pub fn str_shared(&self, contents: Box<str>) -> Str {
    // invariant: if there is a str idx for the contents, always return that instead of allocating
    match contents.as_ref().parse::<BuiltinStr>() {
      Ok(bs) => Str(StrRepr::Copy(CopyStrRepr::Builtin(bs))),
      Err(_) => match self.data_to_idx.get(contents.as_ref()) {
        Some(idx) => Str(StrRepr::Copy(CopyStrRepr::Idx(*idx))),
        None => Str(StrRepr::Alloc(contents)),
      },
    }
  }

  pub fn id(&mut self, contents: Box<str>) -> Id {
    Id(self.mk_copy_repr(contents))
  }

  fn get_idx(&self, idx: StrIdx) -> &str {
    &self.idx_to_data[idx.to_usize()]
  }

  #[must_use]
  pub fn get<'a>(&'a self, s: &'a Str) -> &'a str {
    match &s.0 {
      StrRepr::Copy(repr) => match repr {
        CopyStrRepr::Builtin(b) => b.as_static_str(),
        CopyStrRepr::Idx(idx) => self.get_idx(*idx),
      },
      StrRepr::Alloc(s) => s.as_ref(),
    }
  }
}

/// An identifier.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Id(CopyStrRepr);

impl Id {
  pub fn apply(&mut self, subst: &Subst) {
    self.0.apply(subst);
  }

  pub(crate) const fn builtin(bs: BuiltinStr) -> Self {
    Self(CopyStrRepr::Builtin(bs))
  }
}

#[derive(Debug, Default)]
pub struct Arenas {
  pub str: StrArena,
  pub expr: ExprArena,
}
