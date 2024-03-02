//! The server, which contains language server specific state.

use crate::convert;
use always::always;
use anyhow::{bail, Result};
use paths::FileSystem as _;

#[derive(Debug, Default)]
pub(crate) struct Server<S> {
  pub(crate) fs: paths::RealFileSystem,
  pub(crate) st: S,
  pub(crate) queue: lsp_server::ReqQueue<(), ()>,
  pub(crate) file_watch: bool,
  pub(crate) open_files: paths::PathMap<String>,
}

impl<S> Server<S> {
  pub(crate) fn new(fs: paths::RealFileSystem, st: S) -> Self {
    Self {
      fs,
      st,
      queue: lsp_server::ReqQueue::default(),
      file_watch: false,
      open_files: paths::PathMap::default(),
    }
  }
  pub(crate) fn canonical_path_buf(&self, url: &lsp_types::Url) -> Result<paths::CanonicalPathBuf> {
    let path = convert::path_buf(url)?;
    Ok(self.fs.canonical(path.as_path())?)
  }

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
}

fn send(conn: &lsp_server::Connection, m: lsp_server::Message) {
  match conn.sender.send(m) {
    Ok(()) => {}
    Err(e) => {
      always!(false, "send failure: {e}");
    }
  }
}

pub(crate) fn notify<N>(conn: &lsp_server::Connection, params: N::Params)
where
  N: lsp_types::notification::Notification,
{
  let notif = lsp_server::Notification::new(N::METHOD.to_owned(), params);
  send(conn, notif.into());
}

pub(crate) fn diagnose(
  conn: &lsp_server::Connection,
  paths: &paths::Store,
  ds_map: paths::PathMap<Vec<diagnostic::Diagnostic>>,
) {
  for (path, ds) in ds_map {
    let path = paths.get_path(path);
    let Some(uri) = convert::url(path) else { continue };
    let diagnostics: Vec<_> = ds.into_iter().map(convert::diagnostic).collect();
    let params = lsp_types::PublishDiagnosticsParams { uri, diagnostics, version: None };
    notify::<lsp_types::notification::PublishDiagnostics>(conn, params);
  }
}
