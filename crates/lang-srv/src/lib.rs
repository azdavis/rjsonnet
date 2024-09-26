//! A generic language server.

mod capabilities;
mod convert;
mod notification;
mod request;
mod response;
mod server;
mod util;

/// Sets up and runs the LSP.
///
/// # Panics
///
/// If things failed that it wouldn't make sense to try to recover from, like starting up the LSP or
/// joining I/O threads.
#[expect(clippy::disallowed_methods)]
pub fn run<S: lang_srv_state::State>() {
  better_panic::Settings::new()
    .message(S::BUG_REPORT_MSG)
    .verbosity(better_panic::Verbosity::Medium)
    .install();

  let (conn, io_threads) = lsp_server::Connection::stdio();

  let server_capabilities = serde_json::to_value(capabilities::get()).expect("get capabilities");
  let init = conn.initialize(server_capabilities).expect("init conn");
  let init: lsp_types::InitializeParams = serde_json::from_value(init).expect("get init");
  let fs = paths::RealFileSystem::default();
  let st = S::new(&fs, init.initialization_options);
  let mut srv = server::Server::new(fs, st);

  let last_workspace_folder = init
    .workspace_folders
    .and_then(|mut xs| {
      let ret = xs.pop();
      if !xs.is_empty() {
        log::warn!("we only support the last workspace folder");
      }
      ret
    })
    .map(|x| x.uri.clone());
  #[expect(deprecated)]
  let root_url = last_workspace_folder.or(init.root_uri).expect("root uri");

  srv.file_watch = init
    .capabilities
    .workspace
    .and_then(|x| x.file_operations?.dynamic_registration)
    .unwrap_or_default();

  if srv.file_watch {
    let watchers = vec![lsp_types::FileSystemWatcher {
      glob_pattern: lsp_types::GlobPattern::Relative(lsp_types::RelativePattern {
        base_uri: lsp_types::OneOf::Right(root_url),
        pattern: S::GLOB.to_owned(),
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

  for msg in &conn.receiver {
    match msg {
      lsp_server::Message::Request(req) => {
        if conn.handle_shutdown(&req).expect("handle shutdown") {
          return;
        }
        match request::handle(&mut srv, req).and_then(|r| srv.respond(&conn, r)) {
          Ok(()) => {}
          Err(e) => log::error!("error: {e}"),
        }
      }
      lsp_server::Message::Response(res) => response::handle(res),
      lsp_server::Message::Notification(notif) => {
        match notification::handle(&mut srv, &conn, notif) {
          Ok(()) => {}
          Err(e) => log::error!("error: {e}"),
        }
      }
    }
  }

  io_threads.join().expect("join io threads");
  log::info!("shut down lsp server");
}
