//! Handling notifications from the client, e.g. "these files changed".

use crate::server::{self, Server};
use crate::{convert, util};
use anyhow::{bail, Result};
use std::ops::ControlFlow;

pub(crate) fn handle<S: lang_srv_state::State>(
  srv: &mut Server<S>,
  conn: &lsp_server::Connection,
  notif: lsp_server::Notification,
) -> Result<()> {
  match go(srv, conn, notif) {
    ControlFlow::Continue(x) => bail!("unhandled notification: {x:?}"),
    ControlFlow::Break(Ok(())) => Ok(()),
    ControlFlow::Break(Err(e)) => bail!("couldn't handle notification: {e:?}"),
  }
}

type ControlFlowResult = ControlFlow<Result<()>, lsp_server::Notification>;

fn go<S: lang_srv_state::State>(
  srv: &mut Server<S>,
  conn: &lsp_server::Connection,
  mut notif: lsp_server::Notification,
) -> ControlFlowResult {
  notif = try_notif::<lsp_types::notification::DidChangeWatchedFiles, _>(notif, |params| {
    let mut updated = Vec::<paths::CleanPathBuf>::new();
    for change in params.changes {
      let path = convert::clean_path_buf(&change.uri)?;
      if srv.st.is_open(path.as_clean_path()) {
        // per docs for DidOpenTextDocument:
        //
        // > The document's truth is now managed by the client and the server must not try to read
        // > the document's truth using the document's uri.
        continue;
      }
      updated.push(path);
    }
    srv.st.mark_as_updated(updated);
    Ok(())
  })?;
  notif = try_notif::<lsp_types::notification::DidOpenTextDocument, _>(notif, |params| {
    let path = convert::clean_path_buf(&params.text_document.uri)?;
    let ds = srv.st.open(&srv.fs, path, params.text_document.text);
    server::diagnose(conn, srv.st.paths(), paths::PathMap::from_iter([ds]));
    Ok(())
  })?;
  notif = try_notif::<lsp_types::notification::DidCloseTextDocument, _>(notif, |params| {
    let path = convert::clean_path_buf(&params.text_document.uri)?;
    let ds = srv.st.close(path);
    server::diagnose(conn, srv.st.paths(), ds);
    Ok(())
  })?;
  notif = try_notif::<lsp_types::notification::DidSaveTextDocument, _>(notif, |_| {
    // we should have already gotten content changes from DidChangeTextDocument
    Ok(())
  })?;
  notif = try_notif::<lsp_types::notification::DidChangeTextDocument, _>(notif, |params| {
    let path = convert::clean_path_buf(&params.text_document.uri)?;
    let changes: Vec<_> = params
      .content_changes
      .into_iter()
      .map(|x| apply_changes::Change { range: x.range.map(convert::text_pos_range), text: x.text })
      .collect();
    let ds = srv.st.update_one(&srv.fs, path, changes);
    server::diagnose(conn, srv.st.paths(), paths::PathMap::from_iter([ds]));
    Ok(())
  })?;
  ControlFlow::Continue(notif)
}

fn try_notif<N, F>(notif: lsp_server::Notification, f: F) -> ControlFlowResult
where
  N: lsp_types::notification::Notification,
  F: FnOnce(N::Params) -> Result<()>,
{
  match notif.extract::<N::Params>(N::METHOD) {
    Ok(x) => ControlFlow::Break(f(x)),
    Err(e) => util::extract_error(e),
  }
}
