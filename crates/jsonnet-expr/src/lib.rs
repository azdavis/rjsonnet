//! Jsonnet expressions.

#![allow(missing_docs)]

mod generated {
  include!(concat!(env!("OUT_DIR"), "/generated.rs"));
}

pub mod arg;
pub mod def;
mod subst;

pub use generated::{std_fn, StdFn};
pub use la_arena::{Arena, ArenaMap, Idx};
pub use subst::Subst;

use generated::{BuiltinStr, NotBuiltinStr};
use rustc_hash::FxHashMap;
use std::{collections::hash_map::Entry, fmt};

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
      ExprData::Prim(prim) => match prim {
        Prim::String(s) => s.apply(subst),
        Prim::Null | Prim::Bool(_) | Prim::Number(_) => {}
      },
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
      ExprData::Object { .. }
      | ExprData::Array(_)
      | ExprData::Subscript { .. }
      | ExprData::If { .. }
      | ExprData::BinaryOp { .. }
      | ExprData::UnaryOp { .. }
      | ExprData::Error(_) => {}
    }
  }
}

/// Displays an expression, sort of. Mostly for debugging. (We already derive Debug.)
///
/// TODO handle precedence better, fix trailing commas
#[must_use]
pub fn display_expr<'a>(
  e: Expr,
  str_ar: &'a StrArena,
  expr_ar: &'a ExprArena,
  ps: &'a paths::Store,
  relative_to: Option<&'a paths::CleanPath>,
) -> impl fmt::Display + 'a {
  DisplayExpr { e, str_ar, expr_ar, ps, relative_to }
}

#[derive(Clone, Copy)]
struct DisplayExpr<'a> {
  e: Expr,
  str_ar: &'a StrArena,
  expr_ar: &'a ExprArena,
  ps: &'a paths::Store,
  relative_to: Option<&'a paths::CleanPath>,
}

impl<'a> DisplayExpr<'a> {
  fn with(self, e: Expr) -> DisplayExpr<'a> {
    DisplayExpr { e, ..self }
  }
}

impl<'a> fmt::Display for DisplayExpr<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let Some(e) = self.e else { return f.write_str("_") };
    match &self.expr_ar[e] {
      ExprData::Prim(prim) => prim.display(self.str_ar).fmt(f),
      ExprData::Object { binds, asserts, fields } => {
        f.write_str("{ ")?;
        for &(id, expr) in binds {
          write!(f, "local {} = {}, ", id.display(self.str_ar), self.with(expr))?;
        }
        for &a in asserts {
          write!(f, "assert {}, ", self.with(a))?;
        }
        for field in fields {
          let key = self.with(field.key);
          let plus = if field.plus { "+" } else { "" };
          let vis = field.vis;
          let val = self.with(field.val);
          write!(f, "{key}{plus}{vis} {val}, ")?;
        }
        f.write_str("}")
      }
      ExprData::ObjectComp { name, body, id, ary } => {
        write!(
          f,
          "{{ [{}]: {} for {} in {} }}",
          self.with(*name),
          self.with(*body),
          id.display(self.str_ar),
          self.with(*ary)
        )
      }
      ExprData::Array(elems) => {
        f.write_str("[")?;
        for &elem in elems {
          write!(f, "{}, ", self.with(elem))?;
        }
        f.write_str("]")
      }
      ExprData::Subscript { on, idx } => write!(f, "{}[{}]", self.with(*on), self.with(*idx)),
      ExprData::Call { func, positional, named } => {
        self.with(*func).fmt(f)?;
        f.write_str("(")?;
        for &arg in positional {
          write!(f, "{}, ", self.with(arg))?;
        }
        for &(name, arg) in named {
          write!(f, "{}={}, ", name.display(self.str_ar), self.with(arg))?;
        }
        f.write_str(")")
      }
      ExprData::Id(x) => x.display(self.str_ar).fmt(f),
      ExprData::Local { binds, body } => {
        f.write_str("local ")?;
        for &(bind, e) in binds {
          write!(f, "{} = {}, ", bind.display(self.str_ar), self.with(e))?;
        }
        write!(f, "; {}", self.with(*body))
      }
      ExprData::If { cond, yes, no } => {
        write!(f, "if {} then {} else {}", self.with(*cond), self.with(*yes), self.with(*no))
      }
      ExprData::BinaryOp { lhs, op, rhs } => {
        write!(f, "{} {} {}", self.with(*lhs), op, self.with(*rhs))
      }
      ExprData::UnaryOp { op, inner } => write!(f, "{}{}", op, self.with(*inner)),
      ExprData::Function { params, body } => {
        f.write_str("function(")?;
        for &(bind, default) in params {
          bind.display(self.str_ar).fmt(f)?;
          if let Some(default) = default {
            write!(f, " = {}", self.with(default))?;
          }
          f.write_str(", ")?;
        }
        f.write_str(")")?;
        self.with(*body).fmt(f)
      }
      ExprData::Error(e) => write!(f, "error {}", self.with(*e)),
      ExprData::Import { kind, path } => {
        // TODO handle escapes
        let mut p = self.ps.get_path(*path).as_path();
        if let Some(r) = self.relative_to {
          p = p.strip_prefix(r.as_path()).unwrap_or(p);
        }
        let p = p.display();
        write!(f, "{kind} \"{p}\"")
      }
    }
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ImportKind {
  Code,
  String,
  Binary,
}

impl fmt::Display for ImportKind {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let s = match self {
      ImportKind::Code => "import",
      ImportKind::String => "importstr",
      ImportKind::Binary => "importbin",
    };
    f.write_str(s)
  }
}

#[derive(Debug, Clone, Copy)]
pub enum Visibility {
  Default,
  Hidden,
  Visible,
}

impl fmt::Display for Visibility {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let s = match self {
      Visibility::Default => ":",
      Visibility::Hidden => "::",
      Visibility::Visible => ":::",
    };
    f.write_str(s)
  }
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
}

impl fmt::Display for BinaryOp {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let s = match self {
      BinaryOp::Mul => "*",
      BinaryOp::Div => "/",
      BinaryOp::Add => "+",
      BinaryOp::Sub => "-",
      BinaryOp::Shl => "<<",
      BinaryOp::Shr => ">>",
      BinaryOp::Lt => "<",
      BinaryOp::LtEq => "<=",
      BinaryOp::Gt => ">",
      BinaryOp::GtEq => ">=",
      BinaryOp::BitAnd => "&",
      BinaryOp::BitXor => "^",
      BinaryOp::BitOr => "|",
    };
    f.write_str(s)
  }
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOp {
  Neg,
  Pos,
  LogicalNot,
  BitNot,
}

impl fmt::Display for UnaryOp {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let s = match self {
      UnaryOp::Neg => "-",
      UnaryOp::Pos => "+",
      UnaryOp::LogicalNot => "!",
      UnaryOp::BitNot => "~",
    };
    f.write_str(s)
  }
}

/// A primitive value.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Prim {
  Null,
  Bool(bool),
  String(Str),
  Number(finite_float::Float),
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

struct DisplayCopyStrRepr<'a> {
  repr: CopyStrRepr,
  ar: &'a StrArena,
}

impl fmt::Display for DisplayCopyStrRepr<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self.repr {
      CopyStrRepr::Builtin(bs) => bs.as_static_str().fmt(f),
      CopyStrRepr::Idx(idx) => self.ar.get_idx(idx).fmt(f),
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
  idx_to_contents: Vec<Box<str>>,
  contents_to_idx: FxHashMap<Box<str>, StrIdx>,
}

impl StrArena {
  /// Should only call this when we know the contents are NOT one of the builtin strings. The
  /// `NotBuiltinStr` argument serves as a witness to this fact.
  fn dangerous_mk_idx(&mut self, contents: Box<str>, _: NotBuiltinStr) -> StrIdx {
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
      Err(_) => match self.contents_to_idx.get(contents.as_ref()) {
        Some(idx) => Str(StrRepr::Copy(CopyStrRepr::Idx(*idx))),
        None => Str(StrRepr::Alloc(contents)),
      },
    }
  }

  pub fn id(&mut self, contents: Box<str>) -> Id {
    Id(self.mk_copy_repr(contents))
  }

  fn get_idx(&self, idx: StrIdx) -> &str {
    &self.idx_to_contents[idx.to_usize()]
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
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Id(CopyStrRepr);

impl Id {
  #[must_use]
  pub fn display(self, ar: &StrArena) -> impl fmt::Display + '_ {
    DisplayCopyStrRepr { repr: self.0, ar }
  }

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
