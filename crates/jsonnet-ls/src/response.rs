use anyhow::Result;

pub(crate) fn handle(res: lsp_server::Response) -> Result<()> {
  log::info!("got response: {res:?}");
  Ok(())
}
