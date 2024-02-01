//! A language server for Jsonnet.

// TODO remove
#![allow(clippy::pedantic)]

mod capabilities;
mod notification;
mod request;
mod response;

const VERSION: &str = env!("CARGO_PKG_VERSION");
const ISSUES_URL: &str = "https://github.com/azdavis/rjsonnet/issues";

fn main() -> anyhow::Result<()> {
  let msg =
    format!("jsonnet-ls ({VERSION}) crashed. We would appreciate a bug report: {ISSUES_URL}");
  better_panic::Settings::new().message(msg).verbosity(better_panic::Verbosity::Medium).install();

  let logger_env = env_logger::Env::default().default_filter_or("info");
  env_logger::try_init_from_env(logger_env).expect("couldn't set up env logger");

  log::info!("start up lsp server");
  let (conn, io_threads) = lsp_server::Connection::stdio();

  let server_capabilities = serde_json::to_value(capabilities::get()).unwrap();
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

fn main_loop(conn: &lsp_server::Connection, _: &lsp_types::InitializeParams) -> anyhow::Result<()> {
  log::info!("starting example main loop");
  for msg in &conn.receiver {
    log::info!("got msg: {msg:?}");
    match msg {
      lsp_server::Message::Request(req) => {
        if conn.handle_shutdown(&req)? {
          return Ok(());
        }
        request::handle(conn, req)?;
      }
      lsp_server::Message::Response(res) => response::handle(res)?,
      lsp_server::Message::Notification(notif) => notification::handle(notif)?,
    }
  }
  Ok(())
}
