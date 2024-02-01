use anyhow::{anyhow, Result};
use std::ops::ControlFlow;

pub(crate) fn handle(conn: &lsp_server::Connection, req: lsp_server::Request) -> Result<()> {
  log::info!("got request: {req:?}");
  match go(req) {
    ControlFlow::Continue(x) => log::error!("unhandled request: {x:?}"),
    ControlFlow::Break(Ok(res)) => conn.sender.send(lsp_server::Message::Response(res))?,
    ControlFlow::Break(Err(e)) => log::error!("error: {e:?}"),
  }
  Ok(())
}

fn go(req: lsp_server::Request) -> ControlFlow<Result<lsp_server::Response>, lsp_server::Request> {
  let req = try_req::<lsp_types::request::HoverRequest, _, _>(req, |id, params| {
    log::info!("got hover request #{id}: {params:?}");
    let result = lsp_types::Hover {
      contents: lsp_types::HoverContents::Markup(lsp_types::MarkupContent {
        kind: lsp_types::MarkupKind::Markdown,
        value: "hi there".to_owned(),
      }),
      range: None,
    };
    let result = serde_json::to_value(result).unwrap();
    Ok(lsp_server::Response { id, result: Some(result), error: None })
  })?;
  ControlFlow::Continue(req)
}

fn try_req<R, T, F>(req: lsp_server::Request, f: F) -> ControlFlow<Result<T>, lsp_server::Request>
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
