//! A language server for Jsonnet.

mod capabilities;
mod convert;
mod notification;
mod request;
mod response;
mod server;

const VERSION: &str = env!("CARGO_PKG_VERSION");
const ISSUES_URL: &str = "https://github.com/azdavis/rjsonnet/issues";

fn main() {
  let msg =
    format!("jsonnet-ls ({VERSION}) crashed. We would appreciate a bug report: {ISSUES_URL}");
  better_panic::Settings::new().message(msg).verbosity(better_panic::Verbosity::Medium).install();

  let logger_env = env_logger::Env::default().default_filter_or("info");
  env_logger::try_init_from_env(logger_env).expect("init logger");

  log::info!("start up lsp server");
  let (conn, io_threads) = lsp_server::Connection::stdio();

  let server_capabilities = serde_json::to_value(capabilities::get()).expect("get capabilities");
  let init = conn.initialize(server_capabilities).expect("init conn");
  let init: lsp_types::InitializeParams = serde_json::from_value(init).expect("get init");
  let mut srv = server::Server::default();

  let root_url = init.root_uri.expect("root url");
  let root_path = convert::path_buf(&root_url).expect("root path");
  let paths: Vec<_> = {
    let wd = walkdir::WalkDir::new(root_path.as_path());
    let iter = wd.into_iter().filter_map(|entry| {
      let entry = entry.ok()?;
      entry.path().extension().is_some_and(|x| x == "jsonnet").then(|| entry.into_path())
    });
    iter.collect()
  };
  srv.st.update_many(&srv.fs, Vec::new(), paths);

  main_loop(srv, &conn);

  io_threads.join().expect("join io threads");
  log::info!("shut down lsp server");
}

fn main_loop(mut srv: server::Server, conn: &lsp_server::Connection) {
  for msg in &conn.receiver {
    match msg {
      lsp_server::Message::Request(req) => {
        if conn.handle_shutdown(&req).expect("handle shutdown") {
          return;
        }
        match request::handle(&mut srv, req) {
          Ok(r) => srv.respond(conn, r).expect("respond"),
          Err(e) => log::error!("error: {e}"),
        }
      }
      lsp_server::Message::Response(res) => response::handle(res),
      lsp_server::Message::Notification(notif) => notification::handle(notif),
    }
  }
}
