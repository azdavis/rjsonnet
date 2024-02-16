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
  mut notif: lsp_server::Notification,
) -> ControlFlowResult {
  notif = try_notif::<lsp_types::notification::DidChangeWatchedFiles, _>(notif, |params| {
    let mut add = Vec::<PathBuf>::new();
    let mut remove = Vec::<PathBuf>::new();
    for change in params.changes {
      let pb = srv.canonical_path(&change.uri)?;
      let id = st.path_id(pb.clone());
      if srv.open_files.contains_key(&id) {
        // per docs for DidOpenTextDocument:
        //
        // > The document's truth is now managed by the client and the server must not try to read
        // > the document's truth using the document's uri.
        continue;
      }
      // TODO cut down on converting back into path buf just to then pass to update_many which
      // re-converts back into path id?
      let path = pb.into_path_buf();
      if change.typ == lsp_types::FileChangeType::CREATED
        || change.typ == lsp_types::FileChangeType::CHANGED
      {
        add.push(path);
      } else if change.typ == lsp_types::FileChangeType::DELETED {
        remove.push(path);
      }
    }
    let ds = st.update_many(&srv.fs, remove, add);
    Server::diagnose(conn, st.paths(), ds);
    Ok(())
  })?;
  notif = try_notif::<lsp_types::notification::DidOpenTextDocument, _>(notif, |params| {
    let path = srv.canonical_path(&params.text_document.uri)?;
    let id = st.path_id(path.clone());
    let ds = st.update_one(&srv.fs, path, &params.text_document.text);
    Server::diagnose(conn, st.paths(), ds);
    srv.open_files.insert(id, params.text_document.text);
    Ok(())
  })?;
  notif = try_notif::<lsp_types::notification::DidCloseTextDocument, _>(notif, |params| {
    let path = srv.canonical_path(&params.text_document.uri)?;
    let id = st.path_id(path);
    srv.open_files.remove(&id);
    Ok(())
  })?;
  notif = try_notif::<lsp_types::notification::DidSaveTextDocument, _>(notif, |_| {
    // we should have already gotten content changes from DidChangeTextDocument
    Ok(())
  })?;
  notif = try_notif::<lsp_types::notification::DidChangeTextDocument, _>(notif, |params| {
    let path = srv.canonical_path(&params.text_document.uri)?;
    let id = st.path_id(path.clone());
    let Some(contents) = srv.open_files.get_mut(&id) else {
      let path = path.as_path().display();
      bail!("got DidChangeTextDocument for non-open file: {path}");
    };
    apply_changes(contents, params.content_changes);
    let ds = st.update_one(&srv.fs, path, contents);
    Server::diagnose(conn, st.paths(), ds);
    Ok(())
  })?;
  ControlFlow::Continue(notif)
}

/// adapted from rust-analyzer.
fn apply_changes(
  contents: &mut String,
  mut content_changes: Vec<lsp_types::TextDocumentContentChangeEvent>,
) {
  // If at least one of the changes is a full document change, use the last of them as the starting
  // point and ignore all previous changes.
  let content_changes = match content_changes.iter().rposition(|change| change.range.is_none()) {
    Some(idx) => {
      *contents = std::mem::take(&mut content_changes[idx].text);
      &content_changes[idx + 1..]
    }
    None => &content_changes[..],
  };
  if content_changes.is_empty() {
    return;
  }

  let mut pos_db = text_pos::PositionDb::new(contents);

  // The changes we got must be applied sequentially, but can cross lines so we have to keep our
  // line index updated. Some clients (e.g. Code) sort the ranges in reverse. As an optimization, we
  // remember the last valid line in the index and only rebuild it if needed. The VFS will normalize
  // the end of lines to `\n`.
  let mut index_valid = u32::MAX;
  for change in content_changes {
    // The None case can't happen as we have handled it above already
    let Some(range) = change.range else { continue };
    if index_valid <= range.end.line {
      pos_db = text_pos::PositionDb::new(contents);
    }
    index_valid = range.start.line;
    if let Some(range) = pos_db.text_range_utf16(convert::text_pos_range(range)) {
      contents.replace_range(std::ops::Range::<usize>::from(range), &change.text);
    }
  }
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
