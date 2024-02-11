use crate::{convert, server::Server, state::State, util};
use anyhow::{bail, Result};
use std::{ops::ControlFlow, path::PathBuf};

pub(crate) fn handle<S: State>(
  srv: &mut Server,
  st: &mut S,
  conn: &lsp_server::Connection,
  notif: lsp_server::Notification,
) -> Result<()> {
  match go(srv, st, conn, notif) {
    ControlFlow::Continue(x) => bail!("unhandled notification: {x:?}"),
    ControlFlow::Break(Ok(())) => Ok(()),
    ControlFlow::Break(Err(e)) => bail!("couldn't handle notification: {e:?}"),
  }
}

type ControlFlowResult = ControlFlow<Result<()>, lsp_server::Notification>;

fn go<S: State>(
  srv: &mut Server,
  st: &mut S,
  conn: &lsp_server::Connection,
  notif: lsp_server::Notification,
) -> ControlFlowResult {
  let notif = try_notif::<lsp_types::notification::DidChangeWatchedFiles, _>(notif, |params| {
    let mut add = Vec::<PathBuf>::new();
    let mut remove = Vec::<PathBuf>::new();
    for change in params.changes {
      let path = convert::path_buf(&change.uri)?;
      if change.typ == lsp_types::FileChangeType::CREATED
        || change.typ == lsp_types::FileChangeType::CHANGED
      {
        add.push(path);
      } else if change.typ == lsp_types::FileChangeType::DELETED {
        remove.push(path);
      }
    }
    let ds_map = st.update_many(&srv.fs, remove, add);
    for (path, ds) in ds_map {
      let path = st.paths().get_path(path);
      let uri = lsp_types::Url::from_file_path(path.as_path()).expect("path to url");
      let diagnostics: Vec<_> = ds.into_iter().map(convert::diagnostic).collect();
      let params = lsp_types::PublishDiagnosticsParams { uri, diagnostics, version: None };
      Server::notify::<lsp_types::notification::PublishDiagnostics>(conn, params);
    }
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