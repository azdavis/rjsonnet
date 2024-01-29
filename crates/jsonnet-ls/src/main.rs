//! A language server for Jsonnet.

use anyhow::{anyhow, Result};
use std::ops::ControlFlow;

const VERSION: &str = env!("CARGO_PKG_VERSION");
const ISSUES_URL: &str = "https://github.com/azdavis/rjsonnet/issues";

fn main() -> Result<()> {
  let msg =
    format!("jsonnet-ls ({VERSION}) crashed. We would appreciate a bug report: {ISSUES_URL}");
  better_panic::Settings::new().message(msg).verbosity(better_panic::Verbosity::Medium).install();

  let logger_env = env_logger::Env::default().default_filter_or("info");
  env_logger::try_init_from_env(logger_env).expect("couldn't set up env logger");

  log::info!("start up lsp server");
  let (conn, io_threads) = lsp_server::Connection::stdio();

  let server_capabilities = serde_json::to_value(capabilities()).unwrap();
  let init = match conn.initialize(server_capabilities) {
    Ok(x) => x,
    Err(e) => {
      if e.channel_is_disconnected() {
        io_threads.join()?;
      }
      return Err(e.into());
    }
  };
  let init: lsp_types::InitializeParams = serde_json::from_value(init).unwrap();

  main_loop(&conn, &init)?;

  io_threads.join()?;
  log::info!("shut down lsp server");
  Ok(())
}

fn capabilities() -> lsp_types::ServerCapabilities {
  lsp_types::ServerCapabilities {
    definition_provider: Some(lsp_types::OneOf::Left(true)),
    ..Default::default()
  }
}

fn main_loop(conn: &lsp_server::Connection, _: &lsp_types::InitializeParams) -> Result<()> {
  log::info!("starting example main loop");
  for msg in &conn.receiver {
    log::info!("got msg: {msg:?}");
    match msg {
      lsp_server::Message::Request(req) => {
        if conn.handle_shutdown(&req)? {
          return Ok(());
        }
        log::info!("got request: {req:?}");
        let out = try_req::<lsp_types::request::GotoDefinition, _>(req, |id, params| {
          log::info!("got gotoDefinition request #{id}: {params:?}");
          let result = Some(lsp_types::GotoDefinitionResponse::Array(Vec::new()));
          let result = serde_json::to_value(result).unwrap();
          let resp = lsp_server::Response { id, result: Some(result), error: None };
          conn.sender.send(lsp_server::Message::Response(resp))?;
          Ok(())
        });
        match out {
          ControlFlow::Continue(x) => log::error!("unhandled request: {x:?}"),
          ControlFlow::Break(Ok(())) => {}
          ControlFlow::Break(Err(e)) => log::error!("error: {e:?}"),
        }
      }
      lsp_server::Message::Response(resp) => {
        log::info!("got response: {resp:?}");
      }
      lsp_server::Message::Notification(not) => {
        log::info!("got notification: {not:?}");
      }
    }
  }
  Ok(())
}

fn try_req<R, F>(req: lsp_server::Request, f: F) -> ControlFlow<Result<()>, lsp_server::Request>
where
  R: lsp_types::request::Request,
  F: FnOnce(lsp_server::RequestId, R::Params) -> Result<()>,
{
  match req.extract::<R::Params>(R::METHOD) {
    Ok((id, params)) => ControlFlow::Break(f(id, params)),
    Err(e) => extract_error(e),
  }
}

fn extract_error<T>(e: lsp_server::ExtractError<T>) -> ControlFlow<Result<()>, T> {
  match e {
    lsp_server::ExtractError::MethodMismatch(x) => ControlFlow::Continue(x),
    lsp_server::ExtractError::JsonError { method, error } => {
      ControlFlow::Break(Err(anyhow!("couldn't deserialize for {method}: {error}")))
    }
  }
}
