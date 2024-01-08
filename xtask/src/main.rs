//! A task runner for the repo based on the [xtask spec](https://github.com/matklad/cargo-xtask).

use pico_args::Arguments;
use std::path::{Path, PathBuf};
use std::{env, fs, process::Command};

#[derive(Debug, Clone, Copy)]
enum Cmd {
  Help,
  Ci,
  Dist,
}

struct CmdSpec {
  name: &'static str,
  desc: &'static str,
  options: &'static [(&'static str, &'static str)],
}

impl Cmd {
  const VALUES: [Cmd; 3] = [Cmd::Help, Cmd::Ci, Cmd::Dist];

  fn spec(self) -> CmdSpec {
    match self {
      Cmd::Help => CmdSpec { name: "help", desc: "show this help", options: &[] },
      Cmd::Ci => CmdSpec { name: "ci", desc: "run various tests", options: &[] },
      Cmd::Dist => CmdSpec {
        name: "dist",
        desc: "make artifacts for distribution",
        options: &[("--release", "build for release")],
      },
    }
  }
}

impl std::str::FromStr for Cmd {
  type Err = ();

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    Cmd::VALUES.iter().find(|c| c.spec().name == s).copied().ok_or(())
  }
}

fn show_help() {
  println!("usage:");
  println!("  cargo xtask <command> [<options>]");
  println!();
  println!("commands:");
  for c in Cmd::VALUES {
    let spec = c.spec();
    println!("  {}", spec.name);
    println!("    {}", spec.desc);
    if !spec.options.is_empty() {
      println!();
      println!("    options:");
      for (name, desc) in spec.options {
        println!("      {name}");
        println!("        {desc}");
      }
    }
  }
}

fn finish_args(args: Arguments) {
  let args = args.finish();
  assert!(args.is_empty());
}

fn run(c: &mut Command) {
  let mut sp = c.spawn().expect("spawn cmd");
  let w = sp.wait().expect("wait for cmd");
  assert!(w.success());
}

fn run_ci() {
  run(Command::new("cargo").args(["build", "--locked"]));
  run(Command::new("cargo").args(["fmt", "--", "--check"]));
  run(Command::new("cargo").args(["clippy", "--locked"]));
  run(Command::new("cargo").args(["test", "--locked"]));
}

#[derive(Debug)]
struct DistArgs {
  release: bool,
}

const LANG_SRV_NAME: &str = "jsonnet-ls";

fn cmd_exe(fst: &str) -> Command {
  if cfg!(windows) {
    let mut ret = Command::new("cmd.exe");
    ret.arg("/c").arg(fst);
    ret
  } else {
    Command::new(fst)
  }
}

fn exe(s: &str) -> String {
  let suffix = env::consts::EXE_SUFFIX;
  format!("{s}{suffix}")
}

fn dist(args: &DistArgs) {
  let mut c = Command::new("cargo");
  c.args(["build", "--locked", "--bin", LANG_SRV_NAME]);
  if args.release {
    c.arg("--release");
  }
  run(&mut c);
  let kind = if args.release { "release" } else { "debug" };
  let mut src: PathBuf = ["target", kind].into_iter().collect();
  let mut dst: PathBuf = ["editors", "vscode", "out"].into_iter().collect();
  // ignore errors if it exists already. if we have permission errors we're about to report them
  // with the create_dir_all anyway
  _ = fs::remove_dir_all(&dst);
  fs::create_dir_all(&dst).expect("create dir");
  src.push(exe(LANG_SRV_NAME).as_str());
  dst.push(exe(LANG_SRV_NAME).as_str());
  fs::copy(&src, &dst).expect("copy");
  assert!(dst.pop());
  assert!(dst.pop());
  env::set_current_dir(&dst).expect("set dir");
  if fs::metadata("node_modules").is_err() {
    run(cmd_exe("npm").arg("ci"));
  }
  run(cmd_exe("npm").args(["run", "check"]));
  run(cmd_exe("npm").args(["run", format!("build-{kind}").as_str()]));
}

fn main() {
  let mut args = Arguments::from_env();
  if args.contains(["-h", "--help"]) {
    show_help();
    return;
  }
  let Some(cmd) = args.subcommand().expect("subcommand") else {
    show_help();
    return;
  };
  let cmd = cmd.parse::<Cmd>().expect("parse cmd");
  let dir = Path::new(env!("CARGO_MANIFEST_DIR")).parent().expect("parent");
  env::set_current_dir(dir).expect("set dir");
  match cmd {
    Cmd::Help => show_help(),
    Cmd::Ci => {
      finish_args(args);
      run_ci();
    }
    Cmd::Dist => {
      let dist_args = DistArgs { release: args.contains("--release") };
      finish_args(args);
      dist(&dist_args);
    }
  }
}
