//! Expectations.

use rustc_hash::FxHashSet;
use std::{collections::BTreeMap, fmt};

/// A map from regions to expectations.
#[derive(Debug)]
pub(crate) struct File {
  /// this is a `BTreeMap` because the iteration order matters for defs and uses.
  inner: BTreeMap<Region, FxHashSet<Expect>>,
}

impl File {
  #[must_use]
  pub(crate) fn new(s: &str) -> Self {
    let mut inner = BTreeMap::<Region, FxHashSet<Expect>>::default();
    for (idx, line) in s.lines().enumerate() {
      let Some((region, expect)) = get_one(idx, line) else { continue };
      inner.entry(region).or_default().insert(expect);
    }
    Self { inner }
  }

  pub(crate) fn get(&self, r: Region) -> Option<&FxHashSet<Expect>> {
    self.inner.get(&r)
  }

  pub(crate) fn iter(&self) -> impl Iterator<Item = (&Region, &Expect)> + '_ {
    self.inner.iter().flat_map(|(r, es)| es.iter().map(move |e| (r, e)))
  }
}

/// See [`get_one`].
const COMMENT_START: &str = "##";

/// Parses expectation comments from a line of text. The line will be the following in order:
///
/// - zero or more of any character
/// - the string `COMMENT_START` (the comment start)
/// - zero or more spaces
/// - one arrow character (^ or v or V)
///   - ^ points at the line above
///   - v points at the line below
///   - V points at the line below, but the range must be zero-width
/// - zero or more non-spaces (the column range for the arrow. usually these are all the same as the
///   arrow character.)
/// - one space
/// - one or more of any character (the message)
/// - zero or more whitespace
///
/// If so, this returns `Some((line, col_range, msg))`, else returns `None`.
///
/// Note the arrows might be a little wonky with non-ascii.
fn get_one(line_n: usize, line_s: &str) -> Option<(Region, Expect)> {
  let (before, inner) = line_s.split_once(COMMENT_START)?;
  let non_space_idx = inner.find(|c| c != ' ')?;
  let inner = &inner[non_space_idx..];
  let (col_range, msg) = inner.split_once(' ')?;
  let msg = msg.trim_end();
  let (line, zero_width) = match col_range.chars().next()? {
    '^' => (line_n - 1, false),
    'v' => (line_n + 1, false),
    'V' => (line_n + 1, true),
    c => panic!("invalid arrow: {c}"),
  };
  let line = u32::try_from(line).ok()?;
  let start = before.len() + COMMENT_START.len() + non_space_idx;
  let end_extra = if zero_width {
    assert_eq!(col_range.len(), 1);
    0
  } else {
    col_range.len()
  };
  let end = start + end_extra;
  let region =
    Region { line, col_start: u32::try_from(start).ok()?, col_end: u32::try_from(end).ok()? };
  Some((region, Expect::new(msg)))
}

/// A region that an expectation comment can point at.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct Region {
  pub(crate) line: u32,
  pub(crate) col_start: u32,
  pub(crate) col_end: u32,
}

impl fmt::Display for Region {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    // don't add 1 for the line because the check strings usually have the first line blank.
    write!(f, "{}:{}..{}", self.line, self.col_start + 1, self.col_end + 1)
  }
}

/// Something expected in a source file.
#[derive(Debug, PartialEq, Eq, Hash)]
pub(crate) struct Expect {
  /// The kind of expectation.
  pub(crate) kind: Kind,
  /// The message for it.
  pub(crate) msg: String,
}

impl Expect {
  fn new(msg: &str) -> Self {
    if let Some(msg) = msg.strip_prefix("def: ") {
      return Self { kind: Kind::Def, msg: msg.to_owned() };
    }
    if let Some(msg) = msg.strip_prefix("use: ") {
      return Self { kind: Kind::Use, msg: msg.to_owned() };
    }
    if let Some(msg) = msg.strip_prefix("diagnostic: ") {
      return Self { kind: Kind::Diagnostic, msg: msg.to_owned() };
    }
    if let Some(msg) = msg.strip_prefix("hover: ") {
      return Self { kind: Kind::Hover, msg: msg.to_owned() };
    }
    panic!("no prefix: {msg}")
  }
}

impl fmt::Display for Expect {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}: {}", self.kind, self.msg)
  }
}

/// A kind of expectation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum Kind {
  /// A definition site.
  Def,
  /// A usage site.
  Use,
  /// A diagnostic.
  Diagnostic,
  /// A hover.
  Hover,
}

impl fmt::Display for Kind {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Kind::Def => f.write_str("def"),
      Kind::Use => f.write_str("use"),
      Kind::Diagnostic => f.write_str("diagnostic"),
      Kind::Hover => f.write_str("hover"),
    }
  }
}
