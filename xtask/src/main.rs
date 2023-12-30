//! A task runner for the repo based on the [xtask spec](https://github.com/matklad/cargo-xtask).

use std::{env, fs, process::Command};

fn cmd_exe(fst: &str) -> Command {
  if cfg!(windows) {
    let mut ret = Command::new("cmd.exe");
    ret.arg("/c").arg(fst);
    ret
  } else {
    Command::new(fst)
  }
}

fn run(c: &mut Command) {
  let mut sp = c.spawn().unwrap();
  let w = sp.wait().unwrap();
  assert!(w.success(), "unsuccessful command: {c:?}");
}
fn main() {
  // TODO parse command line args etc better
  let mut args = env::args_os();
  assert!(args.next().is_some(), "first arg should be the name of the executable");
  assert_eq!(args.next().expect("no subcommand"), "dist", "subcommand was not 'dist'");
  assert!(args.next().is_none(), "extra args");
  env::set_current_dir("editors/vscode").unwrap();
  if fs::metadata("node_modules").is_err() {
    run(cmd_exe("npm").arg("ci"));
  }
  run(cmd_exe("npm").args(["run", "check"]));
  run(cmd_exe("npm").args(["run", "build-debug"]));
}
