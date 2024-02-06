//! A language server for Jsonnet.

mod capabilities;
mod convert;
mod notification;
mod request;
mod response;
mod server;
mod state;

const VERSION: &str = env!("CARGO_PKG_VERSION");
const ISSUES_URL: &str = "https://github.com/azdavis/rjsonnet/issues";

fn run<S: state::State>(st: &mut S) {
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
  srv.file_watch = init
    .capabilities
    .workspace
    .and_then(|x| x.file_operations?.dynamic_registration)
    .unwrap_or_default();

  if srv.file_watch {
    let watchers = vec![lsp_types::FileSystemWatcher {
      glob_pattern: lsp_types::GlobPattern::Relative(lsp_types::RelativePattern {
        base_uri: lsp_types::OneOf::Right(root_url),
        pattern: "**/*.{jsonnet,libsonnet,TEMPLATE}".to_owned(),
      }),
      kind: None,
    }];
    let options = lsp_types::DidChangeWatchedFilesRegistrationOptions { watchers };
    let options = serde_json::to_value(options).expect("get registration options");
    let registration =
      convert::registration::<lsp_types::notification::DidChangeWatchedFiles>(options);
    let params = lsp_types::RegistrationParams { registrations: vec![registration] };
    srv.request::<lsp_types::request::RegisterCapability>(&conn, params);
  }

  let paths: Vec<_> = {
    let wd = walkdir::WalkDir::new(root_path.as_path());
    let iter = wd.into_iter().filter_map(|entry| {
      let entry = entry.ok()?;
      let ext = entry.path().extension()?.to_str()?;
      matches!(ext, "jsonnet" | "libsonnet" | "TEMPLATE").then(|| entry.into_path())
    });
    iter.collect()
  };
  st.update_many(&srv.fs, Vec::new(), paths);

  for msg in &conn.receiver {
    match msg {
      lsp_server::Message::Request(req) => {
        if conn.handle_shutdown(&req).expect("handle shutdown") {
          return;
        }
        match request::handle(&mut srv, st, req) {
          Ok(r) => srv.respond(&conn, r).expect("respond"),
          Err(e) => log::error!("error: {e}"),
        }
      }
      lsp_server::Message::Response(res) => response::handle(res),
      lsp_server::Message::Notification(notif) => notification::handle(notif),
    }
  }

  io_threads.join().expect("join io threads");
  log::info!("shut down lsp server");
}

fn main() {
  let mut st = jsonnet_analyze::St::default();
  run(&mut st);
}
