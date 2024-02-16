use crate::{convert, server::Server, state::State, util};
use always::always;
use anyhow::{bail, Result};
use paths::FileSystem;
use std::ops::ControlFlow;

pub(crate) fn handle<S: State>(
  srv: &mut Server,
  st: &mut S,
  req: lsp_server::Request,
) -> Result<lsp_server::Response> {
  srv.queue.incoming.register(req.id.clone(), ());
  match go(srv, st, req) {
    ControlFlow::Continue(x) => bail!("unhandled request: {x:?}"),
    ControlFlow::Break(Ok(response)) => Ok(response),
    ControlFlow::Break(Err(e)) => bail!("couldn't handle request: {e:?}"),
  }
}

type ControlFlowResult<T, C = lsp_server::Request> = ControlFlow<Result<T>, C>;

fn go<S: State>(
  srv: &mut Server,
  st: &mut S,
  mut req: lsp_server::Request,
) -> ControlFlowResult<lsp_server::Response> {
  req = try_req::<lsp_types::request::HoverRequest, _, _>(req, |id, params| {
    let url = &params.text_document_position_params.text_document.uri;
    let path = convert::path_buf(url)?;
    let path = srv.fs.canonicalize(path.as_path())?;
    let json = st.hover(path)?;
    let result = lsp_types::Hover {
      contents: lsp_types::HoverContents::Markup(lsp_types::MarkupContent {
        kind: lsp_types::MarkupKind::Markdown,
        value: format!("```json\n{json}\n```"),
      }),
      range: None,
    };
    Ok(mk_res::<lsp_types::request::HoverRequest>(id, Some(result)))
  })?;
  ControlFlow::Continue(req)
}

fn try_req<R, T, F>(req: lsp_server::Request, f: F) -> ControlFlowResult<T>
where
  R: lsp_types::request::Request,
  F: FnOnce(lsp_server::RequestId, R::Params) -> Result<T>,
{
  match req.extract::<R::Params>(R::METHOD) {
    Ok((id, params)) => ControlFlow::Break(f(id, params)),
    Err(e) => util::extract_error(e),
  }
}

fn mk_res<R>(id: lsp_server::RequestId, res: R::Result) -> lsp_server::Response
where
  R: lsp_types::request::Request,
{
  let result = match serde_json::to_value(res) {
    Ok(x) => Some(x),
    Err(e) => {
      always!(false, "convert response to json error: {e}");
      None
    }
  };
  lsp_server::Response { id, result, error: None }
}
