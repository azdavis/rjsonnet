use anyhow::Result;

pub(crate) fn handle(notif: lsp_server::Notification) -> Result<()> {
  log::info!("got notification: {notif:?}");
  Ok(())
}
