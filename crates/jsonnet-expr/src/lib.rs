//! Jsonnet expressions.

#![allow(missing_docs)]

mod generated {
  include!(concat!(env!("OUT_DIR"), "/generated.rs"));
}

pub mod arg;

pub use generated::{std_fn, StdFn};
pub use la_arena::{Arena, ArenaMap, Idx};

use always::always;
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
      ExprData::Prim(prim) => match prim {
        Prim::String(s) => s.apply(subst),
        Prim::Null | Prim::Bool(_) | Prim::Number(_) => {}
      },
      ExprData::ObjectComp { id, .. } | ExprData::Id(id) => id.apply(subst),
      ExprData::Local { binds, .. } | ExprData::Call { named: binds, .. } => {
        for (id, _) in binds {
          id.apply(subst);
        }
      }
      ExprData::Function { params, .. } => {
        for (id, _) in params {
          id.apply(subst);
        }
      }
      ExprData::Import { path, .. } => *path = subst.paths[path],
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
      ExprData::Object { asserts, fields } => {
        f.write_str("{ ")?;
        for &a in asserts {
          write!(f, "assert {}; ", self.with(a))?;
        }
        for &(key, vis, val) in fields {
          write!(f, "{}{} {}, ", self.with(key), vis, self.with(val))?;
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
        for &(var, e) in binds {
          write!(f, "{} = {}, ", var.display(self.str_ar), self.with(e))?;
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
        for &(name, default) in params {
          name.display(self.str_ar).fmt(f)?;
          if let Some(default) = default {
            write!(f, " = {}", self.with(default))?;
          }
          f.write_str(", ")?;
        }
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

#[derive(Debug, Clone, Copy)]
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

  /// Delegates to `try_from`, and uses always! to assert the Err case is not hit. But if it is, use
  /// `0.0` instead.
  #[must_use]
  pub fn always_from_f64(n: f64) -> Self {
    match Self::try_from(n) {
      Ok(n) => n,
      Err(e) => {
        always!(false, "infinite: {e}");
        Self(0.0)
      }
    }
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
    if let Some(x) = self.value().partial_cmp(&other.value()) {
      x
    } else {
      always!(false, "should not be NaN");
      std::cmp::Ordering::Equal
    }
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

impl From<usize> for Number {
  fn from(value: usize) -> Self {
    #[allow(clippy::cast_precision_loss)]
    Self(value as f64)
  }
}

impl std::ops::Neg for Number {
  type Output = Self;

  fn neg(self) -> Self::Output {
    Self(-self.0)
  }
}

#[derive(Debug, Clone, Copy)]
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
      CopyStrRepr::Idx(idx) => *idx = subst.strings[&idx],
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

/// A substitution, from combining artifacts.
#[derive(Debug, Default)]
pub struct Subst {
  strings: FxHashMap<StrIdx, StrIdx>,
  paths: FxHashMap<paths::PathId, paths::PathId>,
}

impl Subst {
  /// Combine artifacts and produce a substitution to apply to other things.
  pub fn get(art: &mut Artifacts, other: Artifacts) -> Self {
    let mut ret = Subst::default();
    for (idx, s) in other.strings.idx_to_contents.into_iter().enumerate() {
      let old = StrIdx::from_usize(idx);
      let new = art.strings.dangerous_mk_idx(s, NotBuiltinStr::from_str_arena());
      always!(ret.strings.insert(old, new).is_none());
    }
    art.paths.combine(other.paths, &mut |old, new| {
      always!(ret.paths.insert(old, new).is_none());
    });
    ret
  }

  /// Get the path id from the subst.
  #[must_use]
  pub fn get_path_id(&self, path: paths::PathId) -> paths::PathId {
    self.paths.get(&path).copied().unwrap_or(path)
  }
}
