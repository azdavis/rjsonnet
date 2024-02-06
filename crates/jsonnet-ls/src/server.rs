#[derive(Debug, Default)]
pub(crate) struct Server {
  pub(crate) st: jsonnet_analyze::St,
  pub(crate) fs: paths::RealFileSystem,
}
