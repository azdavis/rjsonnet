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

impl From<Id> for Str {
  fn from(id: Id) -> Self {
    Self(StrRepr::Copy(id.0))
  }
}

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
  Unutterable(u32),
}

impl CopyStrRepr {
  fn apply(&mut self, subst: &Subst) {
    match self {
      CopyStrRepr::Builtin(_) | CopyStrRepr::Unutterable(_) => {}
      CopyStrRepr::Idx(idx) => *idx = subst.get_str_idx(*idx),
    }
  }

  fn display(self, ar: &StrArena) -> impl fmt::Display + use<'_> {
    DisplayCopyStrRepr { repr: self, ar }
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
      CopyStrRepr::Unutterable(idx) => write!(f, "${idx}"),
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

  pub fn id_fresh_unutterable(&mut self) -> Id {
    let ret = Id(CopyStrRepr::Unutterable(self.unutterable_idx));
    self.unutterable_idx += 1;
    ret
  }

  fn get_idx(&self, idx: StrIdx) -> &str {
    &self.idx_to_data[idx.to_usize()]
  }

  /// NOTE this is kinda fake for unutterable strings
  #[must_use]
  pub fn get<'a>(&'a self, s: &'a Str) -> &'a str {
    match s.0 {
      StrRepr::Copy(repr) => self.get_copy_str(repr),
      StrRepr::Alloc(ref s) => s.as_ref(),
    }
  }

  fn get_copy_str(&self, s: CopyStrRepr) -> &str {
    match s {
      CopyStrRepr::Builtin(builtin) => builtin.as_static_str(),
      CopyStrRepr::Idx(idx) => self.get_idx(idx),
      CopyStrRepr::Unutterable(_) => "$_",
    }
  }

  /// NOTE this is kinda fake for unutterable strings
  #[must_use]
  pub fn get_id(&self, id: Id) -> &str {
    self.get_copy_str(id.0)
  }
}

/// An identifier.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Id(CopyStrRepr);

impl Id {
  pub fn apply(&mut self, subst: &Subst) {
    self.0.apply(subst);
  }

  #[must_use]
  pub fn is_unutterable(&self) -> bool {
    matches!(
      self.0,
      CopyStrRepr::Unutterable(_)
        | CopyStrRepr::Builtin(
          BuiltinStr::std_unutterable
            | BuiltinStr::a_unutterable
            | BuiltinStr::b_unutterable
            | BuiltinStr::c_unutterable
            | BuiltinStr::d_unutterable
            | BuiltinStr::e_unutterable
        )
    )
  }

  pub(crate) const fn builtin(bs: BuiltinStr) -> Self {
    Self(CopyStrRepr::Builtin(bs))
  }

  #[must_use]
  pub fn display(self, ar: &StrArena) -> impl fmt::Display + use<'_> {
    self.0.display(ar)
  }
}

/// A substitution, from combining artifacts.
#[derive(Debug, Default)]
pub struct Subst {
  strings: FxHashMap<StrIdx, StrIdx>,
  paths: paths::PathMap<paths::PathId>,
}

impl Subst {
  /// Combine artifacts and produce a substitution to apply to other things.
  pub fn get(this: &mut Artifacts, other: Artifacts) -> Self {
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
    ret
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
