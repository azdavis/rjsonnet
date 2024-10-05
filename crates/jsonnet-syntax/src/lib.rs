//! Concrete syntax for Jsonnet.

#![allow(missing_debug_implementations, missing_docs)]

use ast::AstNode as _;

#[allow(clippy::pedantic, missing_debug_implementations, missing_docs)]
pub mod ast {
  include!(concat!(env!("OUT_DIR"), "/ast.rs"));
}

#[allow(clippy::pedantic, missing_debug_implementations, missing_docs, unsafe_code)]
pub mod kind {
  include!(concat!(env!("OUT_DIR"), "/kind.rs"));
}

#[derive(Debug, Clone)]
pub struct Root {
  green: rowan::GreenNode,
}

impl Root {
  /// Makes a new `Root`. The green node should be an ast root.
  #[must_use]
  pub fn new(green: rowan::GreenNode) -> Self {
    Self { green }
  }

  /// Turns this into a syntax node.
  #[must_use]
  pub fn syntax(self) -> kind::SyntaxNode {
    kind::SyntaxNode::new_root(self.green)
  }

  /// Turns this into an ast root.
  #[must_use]
  pub fn into_ast(self) -> Option<ast::Root> {
    ast::Root::cast(self.syntax())
  }
}

/// TODO fill in?
fn custom_node_range(_: kind::SyntaxNode) -> Option<rowan::TextRange> {
  None
}

/// Returns the node range for the node, which is either a custom node range to allow for better
/// readability or the whole actual range of the node.
#[must_use]
pub fn node_range(node: &kind::SyntaxNode) -> rowan::TextRange {
  custom_node_range(node.clone()).unwrap_or_else(|| node.text_range())
}

/// Returns the parent of the token, with some custom adjustments.
#[must_use]
pub fn token_parent(tok: &kind::SyntaxToken) -> Option<kind::SyntaxNode> {
  let regular = tok.parent()?;
  if regular.kind() == kind::SyntaxKind::Object {
    return regular.parent();
  }
  Some(regular)
}

/// Returns the best token in the node at the offset.
#[must_use]
pub fn node_token(syntax: &kind::SyntaxNode, offset: rowan::TextSize) -> Option<kind::SyntaxToken> {
  let range = syntax.text_range();
  if range.start() > offset || offset > range.end() {
    // ensure precondition.
    return None;
  }
  match syntax.token_at_offset(offset) {
    rowan::TokenAtOffset::None => None,
    rowan::TokenAtOffset::Single(t) => Some(t),
    rowan::TokenAtOffset::Between(t1, t2) => {
      Some(if priority(t1.kind()) >= priority(t2.kind()) { t1 } else { t2 })
    }
  }
}

fn priority(kind: kind::SyntaxKind) -> u8 {
  match kind {
    kind::SyntaxKind::Id => 6,
    kind::SyntaxKind::Dot => 5,
    kind::SyntaxKind::LRound
    | kind::SyntaxKind::RRound
    | kind::SyntaxKind::LCurly
    | kind::SyntaxKind::RCurly
    | kind::SyntaxKind::LSquare
    | kind::SyntaxKind::RSquare => 4,
    kind::SyntaxKind::Comma
    | kind::SyntaxKind::Colon
    | kind::SyntaxKind::Star
    | kind::SyntaxKind::Plus
    | kind::SyntaxKind::Eq => 3,
    kind::SyntaxKind::SingleQuotedString
    | kind::SyntaxKind::SingleQuotedVerbatimString
    | kind::SyntaxKind::DoubleQuotedString
    | kind::SyntaxKind::DoubleQuotedVerbatimString
    | kind::SyntaxKind::TrueKw
    | kind::SyntaxKind::FalseKw
    | kind::SyntaxKind::NullKw
    | kind::SyntaxKind::Number => 1,
    kind::SyntaxKind::Whitespace | kind::SyntaxKind::BlockComment | kind::SyntaxKind::Invalid => 0,
    _ => 2,
  }
}
