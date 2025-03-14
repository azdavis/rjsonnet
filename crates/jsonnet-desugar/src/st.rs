//! The mutable state under which we desugar.

use crate::error::{self, Error};
use always::always;
use jsonnet_syntax::{ast, kind::SyntaxToken};
use rustc_hash::FxHashMap;
use text_size::TextRange;

/// Pointers between arena indices and concrete syntax.
#[derive(Debug, Default)]
pub struct Pointers {
  ptr_to_idx: FxHashMap<ast::SyntaxNodePtr, jsonnet_expr::ExprMust>,
  idx_to_ptr: jsonnet_expr::ExprMap<ast::SyntaxNodePtr>,
}

impl Pointers {
  /// Inserts a new pointer.
  pub(crate) fn insert(&mut self, ptr: ast::SyntaxNodePtr, e: jsonnet_expr::ExprMust) {
    self.ptr_to_idx.insert(ptr, e);
    self.idx_to_ptr.insert(e, ptr);
  }

  /// Gets the syntax node pointer for the expr.
  #[must_use]
  pub fn get_ptr(&self, e: jsonnet_expr::ExprMust) -> Option<ast::SyntaxNodePtr> {
    self.idx_to_ptr.get(e).copied()
  }

  /// Maybe gets an expression for a syntax node pointer.
  #[must_use]
  pub fn get_idx(&self, ptr: ast::SyntaxNodePtr) -> jsonnet_expr::Expr {
    self.ptr_to_idx.get(&ptr).copied()
  }
}

#[derive(Debug, Default)]
pub(crate) struct St {
  arenas: jsonnet_expr::Arenas,
  errors: Vec<Error>,
  pointers: Pointers,
  id_counts: jsonnet_expr::Counter,
  paths: paths::Store,
}

impl St {
  pub(crate) fn str(&mut self, s: &str) -> jsonnet_expr::Str {
    self.arenas.str.str(s.to_owned().into_boxed_str())
  }

  pub(crate) fn id(&mut self, tok: SyntaxToken) -> jsonnet_expr::Id {
    self.arenas.str.id(tok.text().to_owned().into_boxed_str())
  }

  pub(crate) fn expr(
    &mut self,
    ptr: ast::SyntaxNodePtr,
    e: jsonnet_expr::ExprData,
  ) -> jsonnet_expr::ExprMust {
    let ret = self.arenas.expr.alloc(e);
    self.pointers.insert(ptr, ret);
    ret
  }

  pub(crate) fn err<N>(&mut self, node: &N, kind: error::Kind)
  where
    N: ast::AstNode,
  {
    self.err_(node.syntax().text_range(), kind);
  }

  pub(crate) fn err_token(&mut self, tok: &SyntaxToken, kind: error::Kind) {
    self.err_(tok.text_range(), kind);
  }

  fn err_(&mut self, range: TextRange, kind: error::Kind) {
    self.errors.push(Error { range, kind });
  }

  pub(crate) fn set_id_count(&mut self, canonical: jsonnet_expr::ExprMust, count: usize) {
    always!(count > 1, "should have an explicit count gt 1");
    self.id_counts.set(canonical, count);
  }

  pub(crate) fn finish(self, top: jsonnet_expr::Expr) -> Desugar {
    Desugar {
      top,
      arenas: self.arenas,
      pointers: self.pointers,
      paths: self.paths,
      id_counts: self.id_counts,
      errors: self.errors,
    }
  }

  /// Returns a fresh identifier.
  pub(crate) fn fresh(&mut self) -> jsonnet_expr::Id {
    self.arenas.str.id_fresh_unutterable()
  }

  pub(crate) fn path_id(&mut self, p: &paths::CleanPath) -> paths::PathId {
    self.paths.get_id(p)
  }
}

/// The result of desugaring.
#[derive(Debug)]
pub struct Desugar {
  /// The single top-level expression.
  pub top: jsonnet_expr::Expr,
  /// The arenas holding the allocations.
  pub arenas: jsonnet_expr::Arenas,
  /// Pointers between arena indices and concrete syntax.
  pub pointers: Pointers,
  /// The paths store.
  pub paths: paths::Store,
  /// The paths store.
  pub id_counts: jsonnet_expr::Counter,
  /// Errors when desugaring.
  pub errors: Vec<Error>,
}

impl Desugar {
  /// Displays the top-level expression.
  #[must_use]
  pub fn display_top<'a>(
    &'a self,
    relative_to: Option<&'a paths::CleanPath>,
  ) -> impl std::fmt::Display {
    jsonnet_expr::display::expr(
      self.top,
      &self.arenas.str,
      &self.arenas.expr,
      &self.paths,
      relative_to,
    )
  }
}
