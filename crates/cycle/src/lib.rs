//! Detecting cycles.

/// A detector for cycles. Allows self-cycles.
#[derive(Debug, Clone)]
pub struct Detector<T> {
  cur: T,
  in_progress: Vec<T>,
}

impl<T> Detector<T> {
  /// Makes a new detector at an item.
  pub fn new(cur: T) -> Self {
    Self { cur, in_progress: Vec::new() }
  }

  /// Returns the current item we're at.
  pub fn cur(&self) -> &T {
    &self.cur
  }
}

impl<T: Clone + Eq> Detector<T> {
  /// Sets the new item.
  ///
  /// # Errors
  ///
  /// If this would cause a non-self cycle.
  pub fn try_push(mut self, cur: T) -> Result<Self, Cycle<T>> {
    let idx = self.in_progress.iter().position(|x| *x == cur);
    match idx {
      None => {
        self.cur = cur.clone();
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
pub struct Cycle<T> {
  /// The first and last thing in the cycle.
  pub first_and_last: T,
  /// The other stuff in the cycle, in order.
  pub intervening: Vec<T>,
}
