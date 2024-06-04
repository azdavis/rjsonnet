//! See [`Diagnostic`].

/// A diagnostic message about a bit of code.
#[derive(Debug)]
pub struct Diagnostic {
  /// The range of the file this diagnostic applies to.
  pub range: text_pos::RangeUtf16,
  /// The message of the diagnostic.
  pub message: String,
  /// The severity.
  pub severity: Severity,
}

/// How severe the diagnostic is.
#[derive(Debug)]
pub enum Severity {
  /// Should maybe be addressed, but can compile without addressing.
  Warning,
  /// Can't compile unless addressed.
  Error,
}
