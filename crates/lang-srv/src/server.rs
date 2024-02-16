use crate::convert;
use always::always;
use anyhow::{bail, Result};
use lsp_types::Url;
use paths::FileSystem as _;

#[derive(Debug, Default)]
pub(crate) struct Server {
  pub(crate) fs: paths::RealFileSystem,
  pub(crate) queue: lsp_server::ReqQueue<(), ()>,
  pub(crate) file_watch: bool,
  pub(crate) open_files: paths::PathMap<String>,
}

impl Server {
  #[allow(dead_code)]
  pub(crate) fn request<R>(&mut self, conn: &lsp_server::Connection, params: R::Params)
  where
    R: lsp_types::request::Request,
  {
    let req = self.queue.outgoing.register(R::METHOD.to_owned(), params, ());
    send(conn, req.into());
  }

  pub(crate) fn respond(
    &mut self,
    conn: &lsp_server::Connection,
    response: lsp_server::Response,
  ) -> Result<()> {
    match self.queue.incoming.complete(response.id.clone()) {
      Some(()) => send(conn, response.into()),
      None => bail!("respond to non-queued request with {response:?}"),
    }
    Ok(())
  }

  #[allow(dead_code)]
  pub(crate) fn notify<N>(conn: &lsp_server::Connection, params: N::Params)
  where
    N: lsp_types::notification::Notification,
  {
    let notif = lsp_server::Notification::new(N::METHOD.to_owned(), params);
    send(conn, notif.into());
  }

  pub(crate) fn canonical_path(&self, url: &Url) -> Result<paths::CanonicalPathBuf> {
    let path = convert::path_buf(url)?;
    Ok(self.fs.canonicalize(path.as_path())?)
  }
}

fn send(conn: &lsp_server::Connection, m: lsp_server::Message) {
  match conn.sender.send(m) {
    Ok(()) => {}
    Err(e) => {
      always!(false, "send failure: {e}");
    }
  }
}
