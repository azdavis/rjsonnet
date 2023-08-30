use jsonnet_hir::Arenas;

#[derive(Debug, Default)]
pub(crate) struct St {
  arenas: Arenas,
}

impl St {
  pub(crate) fn finish(self) -> Arenas {
    self.arenas
  }
}
