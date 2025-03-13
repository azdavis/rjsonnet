//! Strings.

use crate::Artifacts;
use crate::generated::{BuiltinStr, NotBuiltinStr};
use always::always;
use rustc_hash::FxHashMap;
use std::collections::hash_map::Entry;
use std::fmt;

/// A string, which may be interned.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct Str(StrRepr);

impl Str {
  pub fn apply(&mut self, subst: &Subst) {
    match &mut self.0 {
      StrRepr::Builtin(_) => {}
      StrRepr::Idx(idx) => *idx = subst.get_str_idx(*idx),
    }
  }

  pub(crate) const fn builtin(bs: BuiltinStr) -> Self {
    Self(StrRepr::Builtin(bs))
  }

  fn display(self, ar: &StrArena) -> impl fmt::Display {
    StrReprDisplay { repr: self.0, ar }
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum StrRepr {
  Builtin(BuiltinStr),
  Idx(StrIdx),
}

struct StrReprDisplay<'a> {
  repr: StrRepr,
  ar: &'a StrArena,
}

impl fmt::Display for StrReprDisplay<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self.repr {
      StrRepr::Builtin(bs) => bs.as_static_str().fmt(f),
      StrRepr::Idx(idx) => self.ar.get_idx(idx).fmt(f),
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

  /// inserts the contents if it was not in the arena already
  pub fn str(&mut self, contents: Box<str>) -> Str {
    let repr = match contents.as_ref().parse::<BuiltinStr>() {
      Ok(bs) => StrRepr::Builtin(bs),
      Err(nbs) => StrRepr::Idx(self.dangerous_mk_idx(contents, nbs)),
    };
    Str(repr)
  }

  /// does NOT insert the contents if it was not in the arena already, only gets
  #[must_use]
  pub fn try_str(&self, contents: &str) -> Option<Str> {
    let repr = match contents.parse::<BuiltinStr>() {
      Ok(bs) => StrRepr::Builtin(bs),
      Err(_) => StrRepr::Idx(*self.data_to_idx.get(contents)?),
    };
    Some(Str(repr))
  }

  pub fn id(&mut self, contents: Box<str>) -> Id {
    Id(IdRepr::Str(self.str(contents)))
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
  pub fn get(&self, s: Str) -> &str {
    match s.0 {
      StrRepr::Builtin(builtin) => builtin.as_static_str(),
      StrRepr::Idx(idx) => self.get_idx(idx),
    }
  }

  #[must_use]
  pub fn get_id(&self, id: Id) -> Option<&str> {
    match id.0 {
      IdRepr::Str(s) => Some(self.get(s)),
      IdRepr::Unutterable(_) => None,
    }
  }
}

/// An identifier.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct Id(IdRepr);

impl Id {
  pub fn apply(&mut self, subst: &Subst) {
    self.0.apply(subst);
  }

  #[must_use]
  pub fn is_unutterable(&self) -> bool {
    match self.0 {
      IdRepr::Str(s) => match s.0 {
        StrRepr::Builtin(builtin) => builtin.is_unutterable(),
        StrRepr::Idx(_) => false,
      },
      IdRepr::Unutterable(_) => true,
    }
  }

  pub(crate) const fn builtin(bs: BuiltinStr) -> Self {
    Self(IdRepr::Str(Str(StrRepr::Builtin(bs))))
  }

  #[must_use]
  pub fn display(self, ar: &StrArena) -> impl fmt::Display {
    IdReprDisplay { id: self.0, ar }
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum IdRepr {
  Str(Str),
  Unutterable(u32),
}

impl IdRepr {
  pub fn apply(&mut self, subst: &Subst) {
    match self {
      IdRepr::Str(s) => s.apply(subst),
      IdRepr::Unutterable(idx) => *idx += subst.unutterable_idx_shift,
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
  unutterable_idx_shift: u32,
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
    // if we shift all of the indices in other up by this's current idx they will NOT clash with:
    //
    // - each other since they'll all shift up by the same amount
    // - unutterables in this because there is no unutterable above the current idx.
    //
    // we also need to shift up this's index by the other's idx since other's idx is the number of
    // unutterables we created in other.
    ret.unutterable_idx_shift = this.strings.unutterable_idx;
    this.strings.unutterable_idx += other.strings.unutterable_idx;
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
