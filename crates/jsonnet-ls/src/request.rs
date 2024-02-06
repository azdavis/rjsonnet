use crate::{convert, server};
use anyhow::{anyhow, bail, Result};
use std::ops::ControlFlow;

pub(crate) type ControlFlowResult<T, C = lsp_server::Request> = ControlFlow<Result<T>, C>;

pub(crate) fn handle(
  srv: &mut server::Server,
  req: lsp_server::Request,
) -> Result<lsp_server::Response> {
  log::info!("got request: {req:?}");
  match go(srv, req) {
    ControlFlow::Continue(x) => bail!("unhandled request: {x:?}"),
    ControlFlow::Break(Ok(response)) => Ok(response),
    ControlFlow::Break(Err(e)) => bail!("couldn't handle request: {e:?}"),
  }
}

fn go(
  srv: &mut server::Server,
  req: lsp_server::Request,
) -> ControlFlowResult<lsp_server::Response> {
  let req = try_req::<lsp_types::request::HoverRequest, _, _>(req, |id, params| {
    log::info!("got hover request #{id}: {params:?}");
    let url = &params.text_document_position_params.text_document.uri;
    let path_id = convert::path_id(srv, url)?;
    let json = match srv.st.get_json(path_id) {
      Ok(x) => x,
      Err(e) => bail!("couldn't get json: {}", e.display(srv.st.strings())),
    };
    let json = json.display(srv.st.strings());
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
    Err(e) => extract_error(e),
  }
}

fn extract_error<T, C>(e: lsp_server::ExtractError<C>) -> ControlFlow<Result<T>, C> {
  match e {
    lsp_server::ExtractError::MethodMismatch(x) => ControlFlow::Continue(x),
    lsp_server::ExtractError::JsonError { method, error } => {
      ControlFlow::Break(Err(anyhow!("couldn't deserialize for {method}: {error}")))
    }
  }
}

fn mk_res<R>(id: lsp_server::RequestId, res: R::Result) -> lsp_server::Response
where
  R: lsp_types::request::Request,
{
  let result = serde_json::to_value(res).expect("convert response to json");
  lsp_server::Response { id, result: Some(result), error: None }
}
