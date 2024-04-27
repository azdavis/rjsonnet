//! Handling notifications from the client, e.g. "these files changed".

use crate::server::{self, Server};
use crate::{convert, state::State, util};
use anyhow::{bail, Result};
use std::ops::ControlFlow;

pub(crate) fn handle<S: State>(
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

fn go<S: State>(
  srv: &mut Server<S>,
  conn: &lsp_server::Connection,
  mut notif: lsp_server::Notification,
) -> ControlFlowResult {
  notif = try_notif::<lsp_types::notification::DidChangeWatchedFiles, _>(notif, |params| {
    let mut add = Vec::<paths::CleanPathBuf>::new();
    let mut remove = Vec::<paths::CleanPathBuf>::new();
    for change in params.changes {
      let path = convert::clean_path_buf(&change.uri)?;
      let id = srv.st.path_id(path.clone());
      if srv.open_files.contains_key(&id) {
        // per docs for DidOpenTextDocument:
        //
        // > The document's truth is now managed by the client and the server must not try to read
        // > the document's truth using the document's uri.
        continue;
      }
      // TODO cut down on converting back into path buf just to then pass to update_many which
      // re-converts back into path id?
      // let path = path.into_path_buf();
      if change.typ == lsp_types::FileChangeType::CREATED
        || change.typ == lsp_types::FileChangeType::CHANGED
      {
        add.push(path);
      } else if change.typ == lsp_types::FileChangeType::DELETED {
        remove.push(path);
      }
    }
    let ds = srv.st.update_many(&srv.fs, remove, add);
    server::diagnose(conn, srv.st.paths(), ds);
    Ok(())
  })?;
  notif = try_notif::<lsp_types::notification::DidOpenTextDocument, _>(notif, |params| {
    let path = convert::clean_path_buf(&params.text_document.uri)?;
    let id = srv.st.path_id(path.clone());
    let ds = srv.st.update_one(&srv.fs, path, &params.text_document.text);
    server::diagnose(conn, srv.st.paths(), ds);
    srv.open_files.insert(id, params.text_document.text);
    Ok(())
  })?;
  notif = try_notif::<lsp_types::notification::DidCloseTextDocument, _>(notif, |params| {
    let path = convert::clean_path_buf(&params.text_document.uri)?;
    let id = srv.st.path_id(path);
    srv.open_files.remove(&id);
    Ok(())
  })?;
  notif = try_notif::<lsp_types::notification::DidSaveTextDocument, _>(notif, |_| {
    // we should have already gotten content changes from DidChangeTextDocument
    Ok(())
  })?;
  notif = try_notif::<lsp_types::notification::DidChangeTextDocument, _>(notif, |params| {
    let path = convert::clean_path_buf(&params.text_document.uri)?;
    let id = srv.st.path_id(path.clone());
    let Some(contents) = srv.open_files.get_mut(&id) else {
      let path = path.as_path().display();
      bail!("got DidChangeTextDocument for non-open file: {path}");
    };
    let changes: Vec<_> = params
      .content_changes
      .into_iter()
      .map(|x| apply_changes::Change { range: x.range.map(convert::text_pos_range), text: x.text })
      .collect();
    apply_changes::get(contents, changes);
    let ds = srv.st.update_one(&srv.fs, path, contents);
    server::diagnose(conn, srv.st.paths(), ds);
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
