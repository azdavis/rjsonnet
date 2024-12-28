//! Suggestions for possible misspellings/confusions.

use jsonnet_expr::Str;

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

pub(crate) fn approx<'a, I>(
  str_ar: &jsonnet_expr::StrArena,
  target: &str,
  candidates: I,
) -> Option<Str>
where
  I: Iterator<Item = &'a Str>,
{
  let mut candidates: Vec<_> = candidates
    .filter_map(|c| {
      let c_s = str_ar.get(c);
      let n = strsim::normalized_damerau_levenshtein(target, c_s);
      // arbitrary-ish threshold
      if n <= 0.7 {
        return None;
      }
      let n = finite_float::Float::always_from_f64(n);
      Some((std::cmp::Reverse(n), c))
    })
    .collect();
  candidates.sort_unstable();
  candidates.first().map(|&(_, c)| c.clone())
}
