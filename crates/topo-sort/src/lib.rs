//! Stuff for topo sort.

#![allow(missing_docs)]

use always::always;
use std::collections::{BTreeSet, HashSet};
use std::hash::{BuildHasherDefault, Hash, Hasher};

#[derive(Debug)]
pub enum ActionKind {
  Start,
  End,
}

#[derive(Debug)]
pub struct Action<T>(pub T, pub ActionKind);

impl<T> Action<T> {
  pub const fn start(value: T) -> Self {
    Self(value, ActionKind::Start)
  }

  pub const fn end(value: T) -> Self {
    Self(value, ActionKind::End)
  }
}

#[derive(Debug)]
pub struct Work<T>(Vec<Action<T>>);

impl<T> Default for Work<T> {
  fn default() -> Self {
    Self(Vec::new())
  }
}

impl<T> Work<T> {
  pub fn push(&mut self, value: T) {
    self.0.push(Action::start(value));
  }
}

pub trait Visitor {
  type Elem: Copy;
  type Data;
  type Set: Set<Self::Elem>;
  fn enter(&mut self, value: Self::Elem) -> Option<Self::Data>;
  fn process(&mut self, value: Self::Elem, data: Self::Data, work: &mut Work<Self::Elem>);
  fn exit(&mut self, value: Self::Elem, level_idx: usize);
}

pub trait Set<T> {
  fn new() -> Self;
  fn contains(&self, value: T) -> bool;
  fn insert(&mut self, value: T) -> bool;
  fn remove(&mut self, value: T) -> bool;
  fn is_empty(&self) -> bool;
}

impl<T> Set<T> for BTreeSet<T>
where
  T: Ord,
{
  fn new() -> Self {
    BTreeSet::new()
  }

  fn contains(&self, value: T) -> bool {
    self.contains(&value)
  }

  fn insert(&mut self, value: T) -> bool {
    self.insert(value)
  }

  fn remove(&mut self, value: T) -> bool {
    self.remove(&value)
  }

  fn is_empty(&self) -> bool {
    self.is_empty()
  }
}

impl<T, S> Set<T> for HashSet<T, BuildHasherDefault<S>>
where
  T: Hash + Eq,
  S: Hasher + Default,
{
  fn new() -> Self {
    HashSet::default()
  }

  fn contains(&self, value: T) -> bool {
    self.contains(&value)
  }

  fn insert(&mut self, value: T) -> bool {
    self.insert(value)
  }

  fn remove(&mut self, value: T) -> bool {
    self.remove(&value)
  }

  fn is_empty(&self) -> bool {
    self.is_empty()
  }
}

pub fn run<V>(visitor: &mut V, mut work: Work<V::Elem>) -> TopoSort<V::Set, V::Elem>
where
  V: Visitor,
{
  let mut cur = V::Set::new();
  let mut done = V::Set::new();
  // INVARIANT: `level_idx` == how many `End`s are in `work`.
  let mut level_idx = 0usize;
  let mut cycle = None::<V::Elem>;
  while let Some(Action(value, kind)) = work.0.pop() {
    match kind {
      ActionKind::Start => {
        if done.contains(value) {
          continue;
        }
        let Some(data) = visitor.enter(value) else { continue };
        if !cur.insert(value) {
          if cycle.is_none() {
            cycle = Some(value);
          }
          continue;
        }
        work.0.push(Action::end(value));
        level_idx += 1;
        visitor.process(value, data, &mut work);
      }
      ActionKind::End => {
        level_idx = match level_idx.checked_sub(1) {
          None => {
            always!(false, "`End` should have a matching `Start`");
            continue;
          }
          Some(x) => x,
        };
        always!(cur.remove(value), "should only `End` when in `cur`");
        always!(done.insert(value), "should not `End` if already done");
        visitor.exit(value, level_idx);
      }
    }
  }
  always!(level_idx == 0, "should return to starting level");
  always!(cur.is_empty(), "should not have any in progress");
  TopoSort { done, cycle }
}

#[derive(Debug)]
pub struct TopoSort<S, T> {
  pub done: S,
  pub cycle: Option<T>,
}
