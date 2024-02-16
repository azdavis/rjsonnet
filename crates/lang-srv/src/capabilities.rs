pub(crate) fn get() -> lsp_types::ServerCapabilities {
  lsp_types::ServerCapabilities {
    text_document_sync: Some(lsp_types::TextDocumentSyncCapability::Options(
      lsp_types::TextDocumentSyncOptions {
        open_close: Some(true),
        change: None,
        will_save: Some(false),
        will_save_wait_until: Some(false),
        save: None,
      },
    )),
    hover_provider: Some(lsp_types::HoverProviderCapability::Simple(true)),
    ..Default::default()
  }
}
