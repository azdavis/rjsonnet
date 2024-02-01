//! Concrete syntax for Jsonnet.

#![allow(missing_debug_implementations, missing_docs)]

#[allow(clippy::pedantic, missing_debug_implementations, missing_docs)]
pub mod ast {
  include!(concat!(env!("OUT_DIR"), "/ast.rs"));
}

#[allow(clippy::pedantic, missing_debug_implementations, missing_docs)]
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
  /// Turns this into an ast root.
  ///
  /// # Panics
  ///
  /// If this wasn't a root.
  #[must_use]
  pub fn into_ast(self) -> ast::Root {
    use ast::AstNode as _;
    let node = rowan::SyntaxNode::new_root(self.green.clone());
    ast::Root::cast(node).expect("must be a Root")
  }
}
