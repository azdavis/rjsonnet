//! Expectations.

use lang_srv_state::State as _;
use rustc_hash::{FxHashMap, FxHashSet};
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

  pub(crate) fn iter(&self) -> impl Iterator<Item = (&Region, &Expect)> {
    self.inner.iter().flat_map(|(r, es)| es.iter().map(move |e| (r, e)))
  }
}

/// See [`get_one`].
const COMMENT_START: &str = "##";

/// Parses expectation comments from a line of text.
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
    let Some((lhs, rhs)) = msg.split_once(':') else { panic!("no prefix: {msg}") };
    Expect { kind: lhs.parse().expect("not a kind"), msg: rhs.trim().to_owned() }
  }

  #[expect(clippy::too_many_arguments)]
  #[track_caller]
  pub(crate) fn check<F>(
    &self,
    region: &Region,
    st: &mut jsonnet_analyze::St,
    fs: &F,
    path: &paths::CleanPath,
    path_str: &str,
    expects: &paths::PathMap<File>,
    ds_map: &mut FxHashMap<text_pos::RangeUtf16, FxHashSet<String>>,
  ) where
    F: Sync + paths::FileSystem,
  {
    match self.kind {
      Kind::Def => {}
      Kind::Use => {
        let pos = text_pos::PositionUtf16 { line: region.line, col: region.col_start };
        let (def_path, range) = st.get_def(fs, path.to_owned(), pos).expect("no def");
        assert_eq!(range.start.line, range.end.line, "{path_str}: range spans many lines");
        let region =
          Region { line: range.start.line, col_start: range.start.col, col_end: range.end.col };
        let def_exs = expects[&def_path].get(region).expect("nothing at def site");
        let def_ex = Expect { kind: Kind::Def, msg: self.msg.clone() };
        let msg = self.msg.as_str();
        assert!(def_exs.contains(&def_ex), "{path_str}: no def found for {msg}");
      }
      Kind::Diagnostic => {
        let range = text_pos::RangeUtf16 {
          start: text_pos::PositionUtf16 { line: region.line, col: region.col_start },
          end: text_pos::PositionUtf16 { line: region.line, col: region.col_end },
        };
        let got = ds_map.get_mut(&range);
        let Some(range_map) = got else { panic!("{path_str}:{range}: no diagnostics") };
        let want = self.msg.as_str();
        assert!(
          range_map.remove(want),
          "{path_str}:{range}: no diagnostic matches: {want}; available: {range_map:?}"
        );
        if range_map.is_empty() {
          assert!(ds_map.remove(&range).expect("just got it").is_empty());
        }
      }
      Kind::Hover => {
        let pos = text_pos::PositionUtf16 { line: region.line, col: region.col_start };
        let got = st.hover(fs, path.to_owned(), pos);
        let want = self.msg.as_str();
        match (want, got) {
          ("<none>", Some(got)) => panic!("{path_str}:{pos}: wanted no hover, got {got}"),
          ("<none>", None) => {}
          (_, None) => panic!("{path_str}:{pos}: wanted hover, got none"),
          (_, Some(got)) => {
            assert!(
              got.lines().any(|line| line == want),
              "{path_str}:{pos}: none of the lines were equal to '{want}':\n\n{got}"
            );
          }
        }
      }
      Kind::Completions => {
        let pos = text_pos::PositionUtf16 { line: region.line, col: region.col_start };
        let got = st.completions(fs, path.to_owned(), pos);
        let Some(got) = got else { panic!("{path_str}:{pos}: no completions") };
        let mut iter = got.iter();
        let mut got = String::new();
        let fst = iter.next().expect("should not have empty completions");
        got.push_str(fst.name.as_str());
        got.push_str(": ");
        got.push_str(fst.ty.as_str());
        for x in iter {
          got.push_str("; ");
          got.push_str(x.name.as_str());
          got.push_str(": ");
          got.push_str(x.ty.as_str());
        }
        let want = self.msg.as_str();
        assert_eq!(want, got, "{path_str}:{pos}: completions did not match");
      }
    }
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
  /// A diagnostic. Aliased as "err", even when the diagnostic is NOT an error.
  Diagnostic,
  /// A hover. Aliased as "type".
  Hover,
  /// Completions that should be available.
  Completions,
}

impl Kind {
  fn as_str(self) -> &'static str {
    match self {
      Kind::Def => "def",
      Kind::Use => "use",
      Kind::Diagnostic => "diagnostic",
      Kind::Hover => "hover",
      Kind::Completions => "completions",
    }
  }
}

impl std::str::FromStr for Kind {
  type Err = String;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    let ret = match s {
      "def" => Kind::Def,
      "use" => Kind::Use,
      "diagnostic" | "err" => Kind::Diagnostic,
      "hover" | "type" => Kind::Hover,
      "completions" => Kind::Completions,
      _ => return Err(s.to_owned()),
    };
    Ok(ret)
  }
}

impl fmt::Display for Kind {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.write_str(self.as_str())
  }
}
