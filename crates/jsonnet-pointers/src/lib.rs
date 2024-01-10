//! Pointers between concrete syntax and HIR expressions.

use jsonnet_syntax::ast;
use rustc_hash::FxHashMap;

/// Pointers between arena indices and concrete syntax.
#[derive(Debug, Default)]
pub struct Pointers {
  ptr_to_idx: FxHashMap<ast::SyntaxNodePtr, jsonnet_expr::ExprMust>,
  idx_to_ptr: jsonnet_expr::ArenaMap<jsonnet_expr::ExprMust, ast::SyntaxNodePtr>,
}

impl Pointers {
  /// Inserts a new pointer.
  pub fn insert(&mut self, ptr: ast::SyntaxNodePtr, e: jsonnet_expr::ExprMust) {
    self.ptr_to_idx.insert(ptr, e);
    self.idx_to_ptr.insert(e, ptr);
  }

  /// Gets the syntax node pointer for the expr. Both must exist.
  #[must_use]
  pub fn get_ptr(&self, e: jsonnet_expr::ExprMust) -> ast::SyntaxNodePtr {
    self.idx_to_ptr[e]
  }

  /// Maybe gets an expression for a syntax node pointer.
  #[must_use]
  pub fn get_idx(&self, ptr: ast::SyntaxNodePtr) -> jsonnet_expr::Expr {
    self.ptr_to_idx.get(&ptr).copied()
  }
}
