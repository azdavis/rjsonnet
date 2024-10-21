//! Handling requests from the client, e.g. "compute the hover text for this area of the document".

use crate::{convert, server::Server, util};
use always::always;
use anyhow::{bail, Result};
use std::ops::ControlFlow;

pub(crate) fn handle<S: lang_srv_state::State>(
  srv: &mut Server<S>,
  req: lsp_server::Request,
) -> Result<lsp_server::Response> {
  srv.queue.incoming.register(req.id.clone(), ());
  match go(srv, req) {
    ControlFlow::Continue(x) => bail!("unhandled request: {x:?}"),
    ControlFlow::Break(Ok(response)) => Ok(response),
    ControlFlow::Break(Err(e)) => bail!("couldn't handle request: {e:?}"),
  }
}

type ControlFlowResult<T, C = lsp_server::Request> = ControlFlow<Result<T>, C>;

fn go<S: lang_srv_state::State>(
  srv: &mut Server<S>,
  mut req: lsp_server::Request,
) -> ControlFlowResult<lsp_server::Response> {
  req = try_req::<lsp_types::request::HoverRequest, _, _>(req, |id, params| {
    let (path, pos) = convert::text_doc_position(&params.text_document_position_params)?;
    let result = srv.st.hover(&srv.fs, path, pos).map(|text| lsp_types::Hover {
      contents: lsp_types::HoverContents::Markup(lsp_types::MarkupContent {
        kind: lsp_types::MarkupKind::Markdown,
        value: text,
      }),
      range: None,
    });
    Ok(mk_res::<lsp_types::request::HoverRequest>(id, result))
  })?;
  req = try_req::<lsp_types::request::Completion, _, _>(req, |id, params| {
    let (path, pos) = convert::text_doc_position(&params.text_document_position)?;
    let completions = srv.st.completions(&srv.fs, path, pos).map(|xs| {
      lsp_types::CompletionResponse::Array(xs.into_iter().map(convert::completion).collect())
    });
    Ok(mk_res::<lsp_types::request::Completion>(id, completions))
  })?;
  req = try_req::<lsp_types::request::GotoDefinition, _, _>(req, |id, params| {
    let (path, pos) = convert::text_doc_position(&params.text_document_position_params)?;
    let result = srv.st.get_def(&srv.fs, path, pos).and_then(|(path_id, range)| {
      let uri = convert::url(srv.st.paths().get_path(path_id))?;
      Some(lsp_types::GotoDefinitionResponse::Scalar(lsp_types::Location {
        uri,
        range: convert::lsp_range(range),
      }))
    });
    Ok(mk_res::<lsp_types::request::GotoDefinition>(id, result))
  })?;
  req = try_req::<lsp_types::request::Formatting, _, _>(req, |id, params| {
    let url = params.text_document.uri;
    let path = convert::clean_path_buf(&url)?;
    let result = srv.st.format(&srv.fs, path, params.options.tab_size).map(|(new_text, end)| {
      let edit = lsp_types::TextEdit {
        range: lsp_types::Range {
          start: lsp_types::Position { line: 0, character: 0 },
          end: convert::lsp_position(end),
        },
        new_text,
      };
      vec![edit]
    });
    Ok(mk_res::<lsp_types::request::Formatting>(id, result))
  })?;
  req = try_req::<lsp_types::request::SignatureHelpRequest, _, _>(req, |id, params| {
    let (path, pos) = convert::text_doc_position(&params.text_document_position_params)?;
    let result = srv.st.signature_help(&srv.fs, path, pos).map(|help| {
      let params = help.params.into_iter().map(|param| lsp_types::ParameterInformation {
        label: lsp_types::ParameterLabel::LabelOffsets([param.start, param.end]),
        documentation: None,
      });
      let signature = lsp_types::SignatureInformation {
        label: help.label,
        documentation: None,
        parameters: Some(params.collect()),
        active_parameter: None,
      };
      lsp_types::SignatureHelp {
        signatures: vec![signature],
        active_signature: Some(0),
        active_parameter: help.active_param,
      }
    });
    Ok(mk_res::<lsp_types::request::SignatureHelpRequest>(id, result))
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
