use crate::convert;

pub(crate) struct Server {
  pub(crate) st: jsonnet_analyze::St,
  pub(crate) fs: paths::RealFileSystem,
}

impl Server {
  pub(crate) fn init(init: lsp_types::InitializeParams) -> anyhow::Result<Self> {
    let fs = paths::RealFileSystem::default();
    let mut st = jsonnet_analyze::St::default();
    let url = init.root_uri.expect("root uri");
    let root_path = convert::canonical_path_buf(&fs, &url)?;
    let wd = walkdir::WalkDir::new(root_path.as_path());
    // let root_id = st.paths_mut().get_id_owned(root_path);
    let paths = wd.into_iter().filter_map(|entry| {
      let entry = entry.ok()?;
      entry.path().extension().is_some_and(|x| x == "jsonnet").then(|| entry.into_path())
    });
    let paths = paths.collect();
    st.update_many(&fs, Vec::new(), paths);
    Ok(Self { st, fs })
  }
}
