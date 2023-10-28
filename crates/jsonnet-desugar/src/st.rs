use crate::error::{self, Error};
use jsonnet_expr::Arenas;
use jsonnet_syntax::{ast::AstNode, kind::SyntaxToken};
use text_size::TextRange;

#[derive(Debug, Default)]
pub(crate) struct St {
  arenas: Arenas,
  errors: Vec<Error>,
  fresh_idx: usize,
}

impl St {
  pub(crate) fn str(&mut self, s: &str) -> jsonnet_expr::Str {
    self.arenas.str.insert(s.to_owned().into_boxed_str())
  }

  pub(crate) fn expr(&mut self, e: jsonnet_expr::ExprData) -> jsonnet_expr::ExprMust {
    self.arenas.expr.alloc(e)
  }

  pub(crate) fn err<N>(&mut self, node: &N, kind: error::Kind)
  where
    N: AstNode,
  {
    self.err_(node.syntax().text_range(), kind);
  }

  pub(crate) fn err_token(&mut self, tok: SyntaxToken, kind: error::Kind) {
    self.err_(tok.text_range(), kind);
  }

  fn err_(&mut self, range: TextRange, kind: error::Kind) {
    self.errors.push(Error { range, kind });
  }

  pub(crate) fn finish(self) -> (Arenas, Vec<Error>) {
    (self.arenas, self.errors)
  }

  /// Returns a fresh identifier.
  pub(crate) fn fresh(&mut self) -> jsonnet_expr::Id {
    let s = format!("${}", self.fresh_idx);
    self.fresh_idx += 1;
    jsonnet_expr::Id::new(self.arenas.str.insert(s.into_boxed_str()))
  }
}
