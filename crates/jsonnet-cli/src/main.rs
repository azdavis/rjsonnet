//! A CLI for static analysis.

use jsonnet_analyze::{Init, St};
use lang_srv_state::State as _;
use paths::FileSystem as _;
use std::process::ExitCode;

fn main() -> ExitCode {
  let n = run();
  if n == 0 {
    println!("no errors!");
    ExitCode::SUCCESS
  } else {
    let s = if n == 1 { "" } else { "s" };
    println!("{n} error{s}");
    ExitCode::FAILURE
  }
}

fn run() -> usize {
  env_logger::init();
  let mut args = pico_args::Arguments::from_env();
  if args.contains(["-h", "--help"]) {
    println!("usage:");
    println!("  jsonnet-cli [<option>...] <file>...");
    println!();
    println!("options:");
    println!("  -h, --help");
    println!("    show this help");
    println!("  --rm-unused");
    println!("    remove unused locals");
    println!();
    return 0;
  }
  let fs = paths::RealFileSystem::default();
  let pwd = match fs.current_dir() {
    Ok(x) => x,
    Err(e) => {
      println!("couldn't get current dir: {e}");
      return 1;
    }
  };
  let mut st = St::init(pwd.clone(), Init::default());
  let mut ret = 0usize;
  let remove_unused = args.contains("--rm-unused");
  for arg in args.finish() {
    let Some(arg) = arg.to_str() else {
      println!("{}: not valid UTF-8", arg.to_string_lossy());
      continue;
    };
    let mut p = pwd.clone();
    p.push(arg);
    let contents = match fs.read_to_string(p.as_path()) {
      Ok(x) => x,
      Err(e) => {
        println!("{arg}: couldn't read path: {e}");
        ret += 1;
        continue;
      }
    };
    let (_, ds) = st.open(&fs, p.clone(), contents);
    if remove_unused {
      if let Some(contents) = st.remove_unused(&fs, p.as_clean_path()) {
        if let Err(e) = std::fs::write(p.as_path(), contents.as_bytes()) {
          println!("{arg}: couldn't write path: {e}");
          ret += 1;
        }
      }
    }
    st.close(p);
    ret += ds.len();
    for d in ds {
      println!("{arg}:{d}");
    }
  }
  ret
}
