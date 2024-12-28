//! Suggestions for possible misspellings/confusions.

/// Returns an exact suggestion from a pre-written table of possible confusions.
pub(crate) fn exact(s: &str) -> Option<&'static str> {
  let ret = match s {
    "True" | "TRUE" | "YES" => "true",
    "False" | "FALSE" | "NO" => "false",
    "undefined" | "None" | "nil" | "NULL" => "null",
    "elif" | "elsif" => "else if",
    "func" | "fn" => "function",
    "var" | "let" | "const" => "local",
    "require" | "include" => "import",
    "this" => "self",
    _ => return None,
  };
  Some(ret)
}

/// Returns the best suggestion from the candidates that passes a certain goodness threshold.
pub(crate) fn approx<'a, I>(target: &str, candidates: I) -> Option<String>
where
  I: Iterator<Item = &'a str>,
{
  let mut candidates: Vec<_> = candidates
    .filter_map(|c| {
      let n = strsim::normalized_damerau_levenshtein(target, c);
      // arbitrary-ish threshold
      if n <= 0.6 {
        return None;
      }
      let n = finite_float::Float::always_from_f64(n);
      Some((std::cmp::Reverse(n), c))
    })
    .collect();
  candidates.sort_unstable();
  candidates.first().map(|&(_, c)| c.to_owned())
}
