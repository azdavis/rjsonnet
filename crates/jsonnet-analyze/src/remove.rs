//! Options for removing unused things.

use jsonnet_syntax::kind::{SyntaxKind, SyntaxToken};
use std::fmt;

/// What unused items to remove.
#[derive(Debug, Default, Clone, Copy)]
pub enum Flavor {
  #[default]
  /// All locals.
  All,
  /// Just imports.
  Imports,
}

impl std::str::FromStr for Flavor {
  type Err = ParseFlavorError;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    let ret = match s {
      "all" => Self::All,
      "imports" => Self::Imports,
      _ => return Err(ParseFlavorError(())),
    };
    Ok(ret)
  }
}

/// An error when parsing a flavor.
#[derive(Debug)]
pub struct ParseFlavorError(());

impl fmt::Display for ParseFlavorError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.write_str("not a valid flavor to remove")
  }
}

impl std::error::Error for ParseFlavorError {}

/// What comments to remove.
#[derive(Debug, Default, Clone, Copy)]
pub struct Comments {
  /// Comments directly above an item.
  pub above: bool,
  /// Comments directly below an item.
  pub below: bool,
}

/// Options for removal.
#[derive(Debug, Default, Clone, Copy)]
pub struct Options {
  /// What items to remove.
  pub flavor: Flavor,
  /// What comments to remove.
  pub comments: Comments,
}

pub(crate) fn trivia_around(
  start: Option<SyntaxToken>,
  end: Option<SyntaxToken>,
  comments: Comments,
) -> Option<text_size::TextRange> {
  let start = std::iter::successors(start, |tok| {
    let tok = tok.prev_token()?;
    can_absorb(&tok, comments.above).then_some(tok)
  });
  let start = start.last()?;
  let end = std::iter::successors(end, |tok| {
    let tok = tok.next_token()?;
    can_absorb(&tok, comments.below).then_some(tok)
  });
  let end = end.last()?;
  Some(text_size::TextRange::new(start.text_range().start(), end.text_range().end()))
}

fn can_absorb(tok: &jsonnet_syntax::kind::SyntaxToken, comments: bool) -> bool {
  match tok.kind() {
    SyntaxKind::Whitespace => !tok.text().contains("\n\n"),
    SyntaxKind::SlashSlashComment | SyntaxKind::HashComment | SyntaxKind::BlockComment => comments,
    _ => false,
  }
}
