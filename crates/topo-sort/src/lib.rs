//! Stuff for topo sort.

#![allow(missing_docs)]

/// A kind of action.
#[derive(Debug)]
pub enum ActionKind {
  Start,
  End,
}

/// An action.
#[derive(Debug)]
pub struct Action<T>(pub T, pub ActionKind);

impl<T> Action<T> {
  pub const fn start(x: T) -> Self {
    Self(x, ActionKind::Start)
  }

  pub const fn end(x: T) -> Self {
    Self(x, ActionKind::End)
  }
}
