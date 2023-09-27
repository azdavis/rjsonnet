use jsonnet_hir::Arenas;
use jsonnet_syntax::{ast::AstNode, kind::SyntaxToken};
use text_size::TextRange;

#[derive(Debug, Default)]
pub(crate) struct St {
  arenas: Arenas,
  errors: Vec<(TextRange, &'static str)>,
}

impl St {
  pub(crate) fn str(&mut self, s: &str) -> jsonnet_hir::Str {
    self.arenas.str.insert(s.to_owned().into_boxed_str())
  }

  pub(crate) fn expr(&mut self, e: jsonnet_hir::ExprData) -> jsonnet_hir::ExprMust {
    self.arenas.expr.alloc(e)
  }

  pub(crate) fn err<N>(&mut self, node: &N, msg: &'static str)
  where
    N: AstNode,
  {
    self.err_(node.syntax().text_range(), msg);
  }

  pub(crate) fn err_token(&mut self, tok: SyntaxToken, msg: &'static str) {
    self.err_(tok.text_range(), msg);
  }

  fn err_(&mut self, range: TextRange, msg: &'static str) {
    self.errors.push((range, msg));
  }

  pub(crate) fn finish(self) -> Arenas {
    self.arenas
  }
}
