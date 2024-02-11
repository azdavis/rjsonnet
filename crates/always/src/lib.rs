//! Like [always-assert](https://github.com/matklad/always-assert) except:
//!
//! - Rust 2021
//! - no `never!` (`std` doesn't have `assert_not!`, why should we?)
//! - `log` instead of `tracing`
//! - no `FORCE`
//! - asks the user to report an issue

#[doc(hidden)]
pub use log::error as __log_error;

/// The url to report issues at.
pub const BUG_REPORT_MSG: &str =
  "please file a bug report: https://github.com/azdavis/rjsonnet/issues";

/// Like `assert!` except only asserts in debug mode. Returns the condition.
#[macro_export]
macro_rules! always {
  ($cond:expr) => {
    $crate::always!($cond, "assertion failed: {}", stringify!($cond))
  };

  ($cond:expr, $fmt:literal $($arg:tt)*) => {{
    let cond = $cond;
    if cfg!(debug_assertions) {
      assert!(cond, $fmt $($arg)*);
    }
    if !cond {
      $crate::__log_error!($fmt $($arg)*);
      $crate::__log_error!("{}", $crate::BUG_REPORT_MSG);
    }
    cond
  }};
}
