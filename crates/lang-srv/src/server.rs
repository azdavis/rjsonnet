use crate::convert;
use always::always;
use anyhow::{bail, Result};

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

  pub(crate) fn diagnose(
    conn: &lsp_server::Connection,
    paths: &paths::Store,
    ds_map: paths::PathMap<Vec<diagnostic::Diagnostic>>,
  ) {
    // we want to fail if the Err type stops being () inside the match. however oddly putting the
    // `allow` directly on that `let` doesn't work. so we put it a bit further out on this `for`.
    #[allow(clippy::single_match_else, clippy::manual_let_else)]
    for (path, ds) in ds_map {
      let path = paths.get_path(path);
      // the `allow`s above should really be right here.
      let uri = match lsp_types::Url::from_file_path(path.as_path()) {
        Ok(x) => x,
        Err(()) => {
          always!(false, "file path to url error");
          continue;
        }
      };
      let diagnostics: Vec<_> = ds.into_iter().map(convert::diagnostic).collect();
      let params = lsp_types::PublishDiagnosticsParams { uri, diagnostics, version: None };
      Self::notify::<lsp_types::notification::PublishDiagnostics>(conn, params);
    }
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
