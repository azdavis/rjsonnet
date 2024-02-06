use crate::server::Server;
use anyhow::{bail, Result};
use lsp_types::Url;

// pub(crate) fn canonical_path_buf<F>(fs: &F, url: &Url) -> Result<paths::CanonicalPathBuf>
// where
//   F: paths::FileSystem,
// {
//   match url.to_file_path() {
//     Ok(pb) => Ok(fs.canonicalize(pb.as_path())?),
//     Err(()) => bail!("couldn't make a URL into a file path: {url}"),
//   }
// }

pub(crate) fn path_buf(url: &Url) -> Result<std::path::PathBuf> {
  if url.scheme() != "file" {
    bail!("not a file url: {url}")
  }
  match url.to_file_path() {
    Ok(pb) => Ok(pb),
    Err(()) => bail!("couldn't make a URL into a file path: {url}"),
  }
}

pub(crate) fn path_id(srv: &mut Server, url: &Url) -> Result<paths::PathId> {
  let path = path_buf(url)?;
  Ok(srv.st.path_id(&srv.fs, &path))
}
