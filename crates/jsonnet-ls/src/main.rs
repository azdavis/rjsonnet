//! A language server for Jsonnet.

use anyhow::{anyhow, Result};
use lsp_server::Connection;
use lsp_server::{ExtractError, Message, Request, RequestId, Response};
use lsp_types::OneOf;
use lsp_types::{
  request::GotoDefinition, GotoDefinitionResponse, InitializeParams, ServerCapabilities,
};
use std::ops::ControlFlow;

const VERSION: &str = env!("CARGO_PKG_VERSION");
const ISSUES_URL: &str = "https://github.com/azdavis/rjsonnet/issues";

fn main() -> Result<()> {
  let msg =
    format!("jsonnet-ls ({VERSION}) crashed. We would appreciate a bug report: {ISSUES_URL}");
  better_panic::Settings::new().message(msg).verbosity(better_panic::Verbosity::Medium).install();

  let logger_env = env_logger::Env::default().default_filter_or("error");
  env_logger::try_init_from_env(logger_env).expect("couldn't set up env logger");

  log::info!("start up lsp server");
  let (connection, io_threads) = Connection::stdio();

  let server_capabilities = serde_json::to_value(capabilities()).unwrap();
  let init = match connection.initialize(server_capabilities) {
    Ok(x) => x,
    Err(e) => {
      if e.channel_is_disconnected() {
        io_threads.join()?;
      }
      return Err(e.into());
    }
  };
  let init: InitializeParams = serde_json::from_value(init).unwrap();

  main_loop(&connection, &init)?;

  io_threads.join()?;
  log::info!("shut down lsp server");
  Ok(())
}

fn capabilities() -> ServerCapabilities {
  ServerCapabilities { definition_provider: Some(OneOf::Left(true)), ..Default::default() }
}

fn main_loop(connection: &Connection, _: &InitializeParams) -> Result<()> {
  log::info!("starting example main loop");
  for msg in &connection.receiver {
    log::info!("got msg: {msg:?}");
    match msg {
      Message::Request(req) => {
        if connection.handle_shutdown(&req)? {
          return Ok(());
        }
        log::info!("got request: {req:?}");
        let out = try_req::<GotoDefinition, _>(req, |id, params| {
          log::info!("got gotoDefinition request #{id}: {params:?}");
          let result = Some(GotoDefinitionResponse::Array(Vec::new()));
          let result = serde_json::to_value(result).unwrap();
          let resp = Response { id, result: Some(result), error: None };
          connection.sender.send(Message::Response(resp))?;
          Ok(())
        });
        match out {
          ControlFlow::Continue(x) => log::error!("unhandled request: {x:?}"),
          ControlFlow::Break(Ok(())) => {}
          ControlFlow::Break(Err(e)) => log::error!("error: {e:?}"),
        }
      }
      Message::Response(resp) => {
        log::info!("got response: {resp:?}");
      }
      Message::Notification(not) => {
        log::info!("got notification: {not:?}");
      }
    }
  }
  Ok(())
}

fn try_req<R, F>(req: Request, f: F) -> ControlFlow<Result<()>, Request>
where
  R: lsp_types::request::Request,
  F: FnOnce(RequestId, R::Params) -> Result<()>,
{
  match req.extract::<R::Params>(R::METHOD) {
    Ok((id, params)) => ControlFlow::Break(f(id, params)),
    Err(e) => extract_error(e),
  }
}

fn extract_error<T>(e: ExtractError<T>) -> ControlFlow<Result<()>, T> {
  match e {
    ExtractError::MethodMismatch(x) => ControlFlow::Continue(x),
    ExtractError::JsonError { method, error } => {
      ControlFlow::Break(Err(anyhow!("couldn't deserialize for {method}: {error}")))
    }
  }
}
