use jsonnet_hir::Arenas;

#[derive(Debug, Default)]
pub(crate) struct St {
  arenas: Arenas,
}

impl St {
  pub(crate) fn str(&mut self, s: &str) -> jsonnet_hir::Str {
    self.arenas.str.insert(s.to_owned().into_boxed_str())
  }

  pub(crate) fn expr(&mut self, e: jsonnet_hir::ExprData) -> jsonnet_hir::ExprMust {
    self.arenas.expr.alloc(e)
  }

  pub(crate) fn finish(self) -> Arenas {
    self.arenas
  }
}
