//! Conversion functions that use `always!` internally.

use crate::always;

/// Convert from usize to u32, using `always!` to assert overflow doesn't happen. But if it does,
/// return `u32::MAX` instead.
#[must_use]
pub fn usize_to_u32(n: usize) -> u32 {
  match u32::try_from(n) {
    Ok(x) => x,
    Err(e) => {
      always!(false, "convert {n} to u32: {e}");
      u32::MAX
    }
  }
}

/// Convert from u32 to usize, using `always!` to assert overflow doesn't happen. But if it does,
/// return `usize::MAX` instead.
#[must_use]
pub fn u32_to_usize(n: u32) -> usize {
  match usize::try_from(n) {
    Ok(x) => x,
    Err(e) => {
      always!(false, "convert {n} to usize: {e}");
      usize::MAX
    }
  }
}
