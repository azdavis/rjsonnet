//! Detecting cycles.

/// A detector for cycles. Allows self-cycles.
#[derive(Debug, Clone)]
pub(crate) struct Detector {
  cur: paths::PathId,
  in_progress: Vec<paths::PathId>,
}

impl Detector {
  /// Makes a new detector at an item.
  pub(crate) fn new(cur: paths::PathId) -> Self {
    Self { cur, in_progress: Vec::new() }
  }

  /// Returns the current item we're at.
  pub(crate) fn cur(&self) -> paths::PathId {
    self.cur
  }

  /// Sets the new item, returning an error if this would cause a non-self cycle.
  pub(crate) fn try_push(mut self, cur: paths::PathId) -> Result<Self, Cycle> {
    let idx = self.in_progress.iter().position(|&x| x == cur);
    match idx {
      None => {
        self.cur = cur;
        self.in_progress.push(cur);
        Ok(self)
      }
      Some(idx) => {
        Err(Cycle { first_and_last: cur, intervening: self.in_progress.split_off(idx + 1) })
      }
    }
  }
}

/// A cycle error.
#[derive(Debug, Clone)]
pub struct Cycle {
  /// The first and last thing in the cycle.
  pub first_and_last: paths::PathId,
  /// The other stuff in the cycle, in order.
  pub intervening: Vec<paths::PathId>,
}
