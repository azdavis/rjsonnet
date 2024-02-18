//! Analyze files from the CLI once, then exit.

use always::always;

fn main() {
  let mut st = jsonnet_analyze::St::default();
  let fs = paths::RealFileSystem::default();
  let mut num_errors = 0usize;
  for arg in std::env::args_os().skip(1) {
    let p = std::path::PathBuf::from(arg);
    let p = match std::fs::canonicalize(p.as_path()) {
      Ok(x) => x,
      Err(e) => {
        eprintln!("{}: cannot canonicalize: {}", p.display(), e);
        continue;
      }
    };
    let Some(p) = paths::AbsPathBuf::try_new(p) else {
      always!(false, "not absolute after canonicalize");
      continue;
    };
    eprintln!("{}", p.as_abs_path().as_path().display());
    let ds_map = st.update_many(&fs, Vec::new(), vec![p]);
    for (path, ds) in ds_map {
      let path = st.paths().get_path(path).as_path().display();
      for d in ds {
        num_errors += 1;
        eprintln!("{path}:{}: {}", d.range, d.message);
      }
    }
  }
  if num_errors == 0 {
    eprintln!("no errors!");
  } else {
    eprintln!("{num_errors} errors");
  }
}
