//! Analyze files from the CLI once, then exit.

use always::always;
use paths::FileSystem;

fn main() {
  #[allow(clippy::disallowed_methods)]
  let pwd = std::env::current_dir().expect("current dir");
  let mut st = jsonnet_analyze::St::default();
  let fs = paths::RealFileSystem::default();
  let mut num_errors = 0usize;
  for arg in std::env::args_os().skip(1) {
    let p = std::path::PathBuf::from(arg);
    let p = match fs.canonical(p.as_path()) {
      Ok(x) => x,
      Err(e) => {
        always!(false, "couldn't make canonical: {e}");
        continue;
      }
    };
    let ds_map = st.update_many(&fs, Vec::new(), vec![p]);
    for (path, ds) in ds_map {
      let path = st.paths().get_path(path).as_path();
      let path = path.strip_prefix(pwd.as_path()).unwrap_or(path);
      let path = path.display();
      for d in ds {
        num_errors += 1;
        eprintln!("{path}:{}: {}", d.range, d.message);
      }
    }
  }
  if num_errors == 0 {
    eprintln!("no errors!");
  } else {
    let s = if num_errors == 1 { "" } else { "s" };
    eprintln!("{num_errors} error{s}");
  }
}
