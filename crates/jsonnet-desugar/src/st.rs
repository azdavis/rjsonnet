use crate::error::{self, Error};
use jsonnet_expr::Arenas;
use jsonnet_syntax::{ast, kind::SyntaxToken};
use rustc_hash::FxHashMap;
use text_size::TextRange;

#[derive(Debug, Default)]
pub struct Pointers {
  ptr_to_idx: FxHashMap<ast::SyntaxNodePtr, jsonnet_expr::ExprMust>,
  idx_to_ptr: jsonnet_expr::ArenaMap<jsonnet_expr::ExprMust, ast::SyntaxNodePtr>,
}

impl Pointers {
  pub(crate) fn insert(&mut self, ptr: ast::SyntaxNodePtr, e: jsonnet_expr::ExprMust) {
    self.ptr_to_idx.insert(ptr.clone(), e);
    self.idx_to_ptr.insert(e, ptr);
  }

  #[must_use]
  pub fn get_ptr(&self, e: jsonnet_expr::ExprMust) -> ast::SyntaxNodePtr {
    self.idx_to_ptr[e].clone()
  }

  #[must_use]
  pub fn get_idx(&self, ptr: ast::SyntaxNodePtr) -> jsonnet_expr::Expr {
    self.ptr_to_idx.get(&ptr).copied()
  }
}

#[derive(Debug, Default)]
pub(crate) struct St {
  arenas: Arenas,
  errors: Vec<Error>,
  pointers: Pointers,
  fresh_idx: usize,
}

impl St {
  pub(crate) fn str(&mut self, s: &str) -> jsonnet_expr::Str {
    self.arenas.str.insert(s.to_owned().into_boxed_str())
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

  pub(crate) fn err_token(&mut self, tok: SyntaxToken, kind: error::Kind) {
    self.err_(tok.text_range(), kind);
  }

  fn err_(&mut self, range: TextRange, kind: error::Kind) {
    self.errors.push(Error { range, kind });
  }

  pub(crate) fn finish(self) -> (Arenas, Pointers, Vec<Error>) {
    (self.arenas, self.pointers, self.errors)
  }

  /// Returns a fresh identifier.
  pub(crate) fn fresh(&mut self) -> jsonnet_expr::Id {
    let s = format!("${}", self.fresh_idx);
    self.fresh_idx += 1;
    jsonnet_expr::Id::new(self.arenas.str.insert(s.into_boxed_str()))
  }
}
