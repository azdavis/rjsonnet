use crate::error::{self, Error};
use jsonnet_expr::Arenas;
use jsonnet_syntax::{ast, kind::SyntaxToken};
use text_size::TextRange;

#[derive(Debug, Default)]
pub(crate) struct St {
  arenas: Arenas,
  errors: Vec<Error>,
  pointers: jsonnet_pointers::Pointers,
  fresh_idx: usize,
  ps: paths::Store,
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

  pub(crate) fn err_token(&mut self, tok: SyntaxToken, kind: error::Kind) {
    self.err_(tok.text_range(), kind);
  }

  fn err_(&mut self, range: TextRange, kind: error::Kind) {
    self.errors.push(Error { range, kind });
  }

  pub(crate) fn finish(self, top: jsonnet_expr::Expr) -> Desugar {
    Desugar { top, arenas: self.arenas, pointers: self.pointers, ps: self.ps, errors: self.errors }
  }

  /// Returns a fresh identifier.
  pub(crate) fn fresh(&mut self) -> jsonnet_expr::Id {
    let s = format!("${}", self.fresh_idx);
    self.fresh_idx += 1;
    self.arenas.str.id(s.into_boxed_str())
  }

  pub(crate) fn path_id(&mut self, p: &paths::CanonicalPathBuf) -> paths::PathId {
    self.ps.get_id(p)
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
  pub pointers: jsonnet_pointers::Pointers,
  /// The paths store.
  pub ps: paths::Store,
  /// Errors when desugaring.
  pub errors: Vec<Error>,
}
