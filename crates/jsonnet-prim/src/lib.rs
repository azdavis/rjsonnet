//! Primitive Jsonnet values.

#![deny(clippy::pedantic, missing_debug_implementations, rust_2018_idioms)]

mod generated {
  include!(concat!(env!("OUT_DIR"), "/generated.rs"));
}

use rustc_hash::FxHashMap;
use std::collections::hash_map::Entry;

/// A primitive value.
#[derive(Debug, Clone, Copy)]
pub enum Prim {
  Null,
  Bool(bool),
  String(Str),
  Number(f64),
}

/// An interned string.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Str(u32);

impl Str {
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

/// An identifier.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Id(Str);

impl Id {
  #[must_use]
  pub fn new(s: Str) -> Self {
    Self(s)
  }
}
