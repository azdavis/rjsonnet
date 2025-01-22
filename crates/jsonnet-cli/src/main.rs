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
  let mut st = St::init(Init::default());
  let fs = paths::RealFileSystem::default();
  let pwd = match fs.current_dir() {
    Ok(x) => x,
    Err(e) => {
      println!("couldn't get current dir: {e}");
      return 1;
    }
  };
  let mut ret = 0usize;
  for arg in std::env::args().skip(1) {
    let mut p = pwd.clone();
    p.push(arg.as_str());
    let contents = match fs.read_to_string(p.as_path()) {
      Ok(x) => x,
      Err(e) => {
        println!("{arg}: couldn't read path: {e}");
        ret += 1;
        continue;
      }
    };
    let (_, ds) = st.open(&fs, p.clone(), contents);
    st.close(p);
    ret += ds.len();
    for d in ds {
      println!("{arg}:{d}");
    }
  }
  ret
}
