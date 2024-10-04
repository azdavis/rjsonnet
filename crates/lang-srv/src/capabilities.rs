//! The capabilities of the server, which we advertise to the client.

pub(crate) fn get() -> lsp_types::ServerCapabilities {
  lsp_types::ServerCapabilities {
    text_document_sync: Some(lsp_types::TextDocumentSyncCapability::Options(
      lsp_types::TextDocumentSyncOptions {
        open_close: Some(true),
        change: Some(lsp_types::TextDocumentSyncKind::INCREMENTAL),
        will_save: Some(false),
        will_save_wait_until: Some(false),
        save: Some(lsp_types::TextDocumentSyncSaveOptions::SaveOptions(lsp_types::SaveOptions {
          include_text: Some(false),
        })),
      },
    )),
    hover_provider: Some(lsp_types::HoverProviderCapability::Simple(true)),
    definition_provider: Some(lsp_types::OneOf::Left(true)),
    ..Default::default()
  }
}
