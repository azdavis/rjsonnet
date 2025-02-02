//! Strings.

use crate::generated::{BuiltinStr, NotBuiltinStr};
use crate::Artifacts;
use always::always;
use rustc_hash::FxHashMap;
use std::collections::hash_map::Entry;
use std::fmt;

/// A string, which may be interned.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Str(StrRepr);

impl Str {
  pub fn apply(&mut self, subst: &Subst) {
    match &mut self.0 {
      StrRepr::Copy(repr) => repr.apply(subst),
    }
  }

  pub(crate) const fn builtin(bs: BuiltinStr) -> Self {
    Self(StrRepr::Copy(CopyStrRepr::Builtin(bs)))
  }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum StrRepr {
  Copy(CopyStrRepr),
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

  fn display(self, ar: &StrArena) -> impl fmt::Display + use<'_> {
    CopyStrReprDisplay { repr: self, ar }
  }
}

struct CopyStrReprDisplay<'a> {
  repr: CopyStrRepr,
  ar: &'a StrArena,
}

impl fmt::Display for CopyStrReprDisplay<'_> {
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
  idx_to_data: Vec<Box<str>>,
  data_to_idx: FxHashMap<Box<str>, StrIdx>,
  unutterable_idx: u32,
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

  pub fn id(&mut self, contents: Box<str>) -> Id {
    Id(IdRepr::Str(self.mk_copy_repr(contents)))
  }

  pub fn id_fresh_unutterable(&mut self) -> Id {
    let ret = Id(IdRepr::Unutterable(self.unutterable_idx));
    self.unutterable_idx += 1;
    ret
  }

  fn get_idx(&self, idx: StrIdx) -> &str {
    &self.idx_to_data[idx.to_usize()]
  }

  #[must_use]
  pub fn get<'a>(&'a self, s: &'a Str) -> &'a str {
    match s.0 {
      StrRepr::Copy(repr) => self.get_copy_str(repr),
    }
  }

  fn get_copy_str(&self, s: CopyStrRepr) -> &str {
    match s {
      CopyStrRepr::Builtin(builtin) => builtin.as_static_str(),
      CopyStrRepr::Idx(idx) => self.get_idx(idx),
    }
  }

  #[must_use]
  pub fn get_id(&self, id: Id) -> Option<&str> {
    match id.0 {
      IdRepr::Str(s) => Some(self.get_copy_str(s)),
      IdRepr::Unutterable(_) => None,
    }
  }
}

/// An identifier.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Id(IdRepr);

impl Id {
  pub fn apply(&mut self, subst: &Subst) {
    self.0.apply(subst);
  }

  #[must_use]
  pub fn is_unutterable(&self) -> bool {
    match self.0 {
      IdRepr::Str(s) => match s {
        CopyStrRepr::Builtin(builtin) => builtin.is_unutterable(),
        CopyStrRepr::Idx(_) => false,
      },
      IdRepr::Unutterable(_) => true,
    }
  }

  pub(crate) const fn builtin(bs: BuiltinStr) -> Self {
    Self(IdRepr::Str(CopyStrRepr::Builtin(bs)))
  }

  #[must_use]
  pub fn display(self, ar: &StrArena) -> impl fmt::Display + use<'_> {
    IdReprDisplay { id: self.0, ar }
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum IdRepr {
  Str(CopyStrRepr),
  Unutterable(u32),
}

impl IdRepr {
  pub fn apply(&mut self, subst: &Subst) {
    match self {
      IdRepr::Str(s) => s.apply(subst),
      IdRepr::Unutterable(_) => {}
    }
  }
}

struct IdReprDisplay<'a> {
  id: IdRepr,
  ar: &'a StrArena,
}

impl fmt::Display for IdReprDisplay<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self.id {
      IdRepr::Str(s) => s.display(self.ar).fmt(f),
      IdRepr::Unutterable(idx) => write!(f, "${idx}"),
    }
  }
}

/// A substitution, from combining artifacts.
#[derive(Debug, Default)]
pub struct Subst {
  strings: FxHashMap<StrIdx, StrIdx>,
  paths: paths::PathMap<paths::PathId>,
}

impl Subst {
  /// Combines artifacts. Then, if that combination produced a non-empty substitution to apply to
  /// other things, return it.
  #[must_use]
  pub fn get(this: &mut Artifacts, other: Artifacts) -> Option<Self> {
    let mut ret = Subst::default();
    for (idx, s) in other.strings.idx_to_data.into_iter().enumerate() {
      let old = StrIdx::from_usize(idx);
      let new = this.strings.dangerous_mk_idx(s, NotBuiltinStr::from_str_arena());
      if old != new {
        always!(ret.strings.insert(old, new).is_none());
      }
    }
    this.paths.combine(other.paths, &mut |old, new| {
      if old != new {
        always!(ret.paths.insert(old, new).is_none());
      }
    });
    (!ret.is_empty()).then_some(ret)
  }

  fn is_empty(&self) -> bool {
    self.strings.is_empty() && self.paths.is_empty()
  }

  /// Get the path id from the subst.
  #[must_use]
  pub fn get_path_id(&self, path: paths::PathId) -> paths::PathId {
    self.paths.get(&path).copied().unwrap_or(path)
  }

  /// Get the path id from the subst.
  #[must_use]
  fn get_str_idx(&self, idx: StrIdx) -> StrIdx {
    self.strings.get(&idx).copied().unwrap_or(idx)
  }
}
