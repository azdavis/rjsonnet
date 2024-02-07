//! See [`Diagnostic`].

/// A diagnostic message about a bit of code.
#[derive(Debug)]
pub struct Diagnostic {
  /// The range of the file this diagnostic applies to.
  pub range: text_pos::RangeUtf16,
  /// The message of the diagnostic.
  pub message: String,
}
