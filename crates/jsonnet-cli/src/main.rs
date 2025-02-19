//! A CLI for static analysis.

use jsonnet_analyze::{Init, St};
use lang_srv_state::State as _;
use paths::FileSystem as _;
use std::process::ExitCode;

fn main() -> ExitCode {
  env_logger::init();
  let mut args = pico_args::Arguments::from_env();
  if args.contains(["-h", "--help"]) {
    println!("usage:");
    println!("  jsonnet-cli [<option>...] <file>...");
    println!();
    println!("options:");
    println!("  -h, --help");
    println!("    show this help");
    println!("  -q, --quiet");
    println!("    emit no output");
    println!("  --rm-unused");
    println!("    remove unused locals");
    println!("  --root-dirs <dirs>");
    println!("    comma-separated extra root directories");
    println!();
    return ExitCode::SUCCESS;
  }
  let rm_unused = args.contains("--rm-unused");
  let quiet = args.contains(["-q", "--quiet"]);
  let root_dirs: Option<String> = match args.opt_value_from_str("--root-dirs") {
    Ok(x) => x,
    Err(e) => {
      println!("failed to parse option: {e}");
      return ExitCode::FAILURE;
    }
  };
  let args = args.finish();
  let n = run(rm_unused, quiet, root_dirs.as_deref(), args);
  if n == 0 {
    if !quiet {
      println!("no errors!");
    }
    ExitCode::SUCCESS
  } else {
    if !quiet {
      let s = if n == 1 { "" } else { "s" };
      println!("{n} error{s}");
    }
    ExitCode::FAILURE
  }
}

fn run(
  rm_unused: bool,
  quiet: bool,
  root_dirs: Option<&str>,
  args: Vec<std::ffi::OsString>,
) -> usize {
  let fs = paths::RealFileSystem::default();
  let pwd = match fs.current_dir() {
    Ok(x) => x,
    Err(e) => {
      if !quiet {
        println!("couldn't get current dir: {e}");
      }
      return 1;
    }
  };
  let root_dirs = root_dirs.iter().flat_map(|x| x.split(',')).map(|x| {
    let mut p = pwd.clone();
    p.push(x);
    p
  });
  let root_dirs: Vec<_> = root_dirs.collect();
  let mut st = St::init(pwd.clone(), Init { root_dirs, ..Default::default() });
  let mut ret = 0usize;
  for arg in args {
    let Some(arg) = arg.to_str() else {
      if !quiet {
        println!("{}: not valid UTF-8", arg.to_string_lossy());
      }
      ret += 1;
      continue;
    };
    let mut p = pwd.clone();
    p.push(arg);
    let contents = match fs.read_to_string(p.as_path()) {
      Ok(x) => x,
      Err(e) => {
        if !quiet {
          println!("{arg}: couldn't read path: {e}");
        }
        ret += 1;
        continue;
      }
    };
    let (_, ds) = st.open(&fs, p.clone(), contents);
    if rm_unused {
      if let Some(contents) = st.remove_unused(&fs, p.as_clean_path()) {
        if let Err(e) = std::fs::write(p.as_path(), contents.as_bytes()) {
          if !quiet {
            println!("{arg}: couldn't write path: {e}");
          }
          ret += 1;
        }
      }
    }
    st.close(p);
    ret += ds.len();
    for d in ds {
      if !quiet {
        println!("{arg}:{d}");
      }
    }
  }
  ret
}
