pub(crate) fn get() -> lsp_types::ServerCapabilities {
  lsp_types::ServerCapabilities {
    hover_provider: Some(lsp_types::HoverProviderCapability::Simple(true)),
    ..Default::default()
  }
}
