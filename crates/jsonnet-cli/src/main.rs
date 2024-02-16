//! Analyze files from the CLI once, then exit.

use std::path::PathBuf;

fn main() {
  let mut st = jsonnet_analyze::St::default();
  let fs = paths::RealFileSystem::default();
  let mut num_errors = 0usize;
  for arg in std::env::args_os().skip(1) {
    let p = PathBuf::from(arg);
    eprintln!("{}", p.display());
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
