//! Analyze files from the CLI once, then exit.

use always::always;
use paths::FileSystem;
use pico_args::Arguments;

fn run() -> usize {
  #[allow(clippy::disallowed_methods)]
  let pwd = std::env::current_dir().expect("current dir");
  let mut args = Arguments::from_env();
  let name_only = args.contains("--name-only");
  let files = args.finish();
  let mut st = jsonnet_analyze::St::default();
  let fs = paths::RealFileSystem::default();
  let mut ret = 0usize;
  for arg in files {
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
      ret += ds.len();
      if name_only {
        eprintln!("{path}");
      } else {
        for d in ds {
          eprintln!("{path}:{}: {}", d.range, d.message);
        }
      }
    }
  }
  ret
}

fn main() {
  let num_errors = run();
  if num_errors == 0 {
    eprintln!("no errors!");
  } else {
    let s = if num_errors == 1 { "" } else { "s" };
    eprintln!("{num_errors} error{s}");
    std::process::exit(1)
  }
}
