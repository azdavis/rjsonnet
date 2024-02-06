use crate::convert;

pub(crate) struct Server {
  pub(crate) st: jsonnet_analyze::St,
  pub(crate) fs: paths::RealFileSystem,
}

impl Server {
  pub(crate) fn init(init: lsp_types::InitializeParams) -> Self {
    let mut ret = Self { st: jsonnet_analyze::St::default(), fs: paths::RealFileSystem::default() };
    let url = init.root_uri.expect("root uri");
    let root_path = convert::path_buf(&url).expect("root path");
    let wd = walkdir::WalkDir::new(root_path.as_path());
    let paths = wd.into_iter().filter_map(|entry| {
      let entry = entry.ok()?;
      entry.path().extension().is_some_and(|x| x == "jsonnet").then(|| entry.into_path())
    });
    let paths = paths.collect();
    ret.st.update_many(&ret.fs, Vec::new(), paths);
    ret
  }
}
