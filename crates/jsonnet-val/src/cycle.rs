//! Detecting cycles.

#[derive(Debug, Clone)]
pub(crate) struct Detector {
  path: paths::PathId,
  cur_paths: Vec<paths::PathId>,
}

impl Detector {
  pub(crate) fn cur(&self) -> paths::PathId {
    self.path
  }

  pub(crate) fn new(path: paths::PathId) -> Self {
    Self { path, cur_paths: Vec::new() }
  }

  pub(crate) fn try_push(&self, path: paths::PathId) -> Result<Self, Cycle> {
    let mut cur_paths = self.cur_paths.clone();
    let idx = cur_paths.iter().position(|&p| p == path);
    match idx {
      None => {
        cur_paths.push(path);
        Ok(Self { path, cur_paths })
      }
      Some(idx) => Err(Cycle { first_and_last: path, intervening: cur_paths.split_off(idx + 1) }),
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
