//! A task runner for the repo based on the [xtask spec](https://github.com/matklad/cargo-xtask).

#![expect(clippy::disallowed_methods)]

use flate2::{write::GzEncoder, Compression};
use pico_args::Arguments;
use std::path::{Path, PathBuf};
use std::{env, fs, io, process::Command};

#[derive(Debug, Clone, Copy)]
enum Cmd {
  Help,
  Ci,
  Dist,
  Release,
}

struct CmdSpec {
  name: &'static str,
  desc: &'static str,
  options: &'static [(&'static str, &'static str)],
}

impl Cmd {
  const VALUES: [Cmd; 4] = [Cmd::Help, Cmd::Ci, Cmd::Dist, Cmd::Release];

  fn spec(self) -> CmdSpec {
    match self {
      Cmd::Help => CmdSpec { name: "help", desc: "show this help", options: &[] },
      Cmd::Ci => CmdSpec { name: "ci", desc: "run various tests", options: &[] },
      Cmd::Dist => CmdSpec {
        name: "dist",
        desc: "make artifacts for distribution",
        options: &[
          ("--release", "build for release"),
          ("--with-cli", "build the CLI as well"),
          ("--target <TARGET>", "a specific target to build for"),
        ],
      },
      Cmd::Release => CmdSpec {
        name: "release",
        desc: "make a new release",
        options: &[("--tag <TAG>", "the new tag to create")],
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
  run(Command::new("cargo").args(["clippy", "--locked", "--tests"]));
  run(Command::new("cargo").args(["test", "--locked"]));
}

#[derive(Debug)]
struct DistArgs {
  release: bool,
  with_cli: bool,
  target: Option<String>,
}

const LANG_SRV_NAME: &str = "jsonnet-ls";
const CLI_NAME: &str = "jsonnet-cli";

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

fn gzip(src: &Path, dst: &Path) {
  let dst_file = fs::File::create(dst).expect("create gzip dist");
  let mut encoder = GzEncoder::new(dst_file, Compression::best());
  let src_file = fs::File::open(src).expect("open file");
  let mut input = io::BufReader::new(src_file);
  io::copy(&mut input, &mut encoder).expect("copy gzip");
  encoder.finish().expect("finish gzip");
}

fn dist(args: &DistArgs) {
  let mut c = Command::new("cargo");
  c.args(["build", "--locked"]);
  if args.release {
    c.arg("--release");
  }
  if let Some(target) = &args.target {
    c.args(["--target", target.as_str()]);
  }
  let targets =
    if args.with_cli { [LANG_SRV_NAME, CLI_NAME].as_slice() } else { [LANG_SRV_NAME].as_slice() };
  for bin in targets {
    c.args(["--bin", bin]);
  }
  run(&mut c);
  let kind = if args.release { "release" } else { "debug" };
  let mut src: PathBuf =
    std::iter::once("target").chain(args.target.as_deref()).chain(std::iter::once(kind)).collect();
  let mut dst: PathBuf;
  if let Some(target) = &args.target {
    dst = PathBuf::from("binary");
    fs::create_dir_all(&dst).expect("create dirs");
    for bin in targets {
      let gz = format!("{bin}-{target}.gz");
      src.push(exe(bin).as_str());
      dst.push(gz.as_str());
      gzip(&src, &dst);
      assert!(src.pop());
      assert!(dst.pop());
    }
  }
  dst = ["editors", "vscode", "out"].into_iter().collect();
  // ignore errors if it exists already. if we have permission errors we're about to report them
  // with the create_dir_all anyway
  _ = fs::remove_dir_all(&dst);
  fs::create_dir_all(&dst).expect("create dir");
  src.push(exe(LANG_SRV_NAME).as_str());
  dst.push(exe(LANG_SRV_NAME).as_str());
  fs::copy(&src, &dst).expect("copy");
  assert!(dst.pop());
  assert!(dst.pop());

  let changelog = include_str!("../../docs/CHANGELOG.md");
  dst.push("CHANGELOG.md");
  fs::write(&dst, changelog).expect("write changelog");
  assert!(dst.pop());

  let license = {
    let header =
      "Millet is licensed under either the MIT license or the Apache license v2.0, at your option.";
    let apache = include_str!("../../LICENSE-APACHE.md");
    let mit = include_str!("../../LICENSE-MIT.md");
    format!("{header}\n\n{apache}\n{mit}")
  };
  dst.push("LICENSE.md");
  fs::write(&dst, license).expect("write combined license");
  assert!(dst.pop());

  dst.push("img");
  fs::create_dir_all(&dst).expect("create img dir");

  for entry in fs::read_dir("img").expect("read img dir") {
    let entry = entry.expect("entry");
    dst.push(entry.file_name());
    fs::copy(entry.path(), &dst).expect("copy img");
    assert!(dst.pop());
  }

  env::set_current_dir(&dst).expect("set dir");
  if fs::metadata("node_modules").is_err() {
    run(cmd_exe("npm").arg("ci"));
  }
  run(cmd_exe("npm").args(["run", "check"]));
  run(cmd_exe("npm").args(["run", format!("build-{kind}").as_str()]));
}

fn modify_each_line<P, F>(path: P, mut f: F)
where
  P: AsRef<Path>,
  F: FnMut(&mut String, usize, &str),
{
  let contents = fs::read_to_string(path.as_ref()).expect("read file");
  let mut out = String::with_capacity(contents.len());
  for (idx, line) in contents.lines().enumerate() {
    f(&mut out, idx, line);
    out.push('\n');
  }
  fs::write(path.as_ref(), out).expect("write file");
}

fn release(tag_arg: &str) {
  let version = tag_arg.strip_prefix('v').expect("tag must start with v");
  let version_parts: Vec<_> = version.split('.').collect();
  let num_parts = version_parts.len();
  assert_eq!(num_parts, 3, "version must have 3 dot-separated parts");
  for part in version_parts {
    part.parse::<u16>().expect("parse as u16");
  }
  let package_json_ish = ["editors/vscode/package.json", "editors/vscode/package-lock.json"];
  for path in &package_json_ish {
    modify_each_line(path, |out, idx, line| {
      if idx >= 15 {
        out.push_str(line);
      } else {
        match line.split_once(": ") {
          None => out.push_str(line),
          Some((key, _)) => {
            if key.trim() == "\"version\"" {
              out.push_str(key);
              out.push_str(": \"");
              out.push_str(version);
              out.push_str("\",");
            } else {
              out.push_str(line);
            }
          }
        }
      }
    });
  }
  let cargo_toml = "Cargo.toml";
  modify_each_line(cargo_toml, |out, _, line| {
    if line.strip_prefix("version = \"").and_then(|x| x.strip_suffix('"')).is_some() {
      out.push_str("version = ");
      out.push('"');
      out.push_str(version);
      out.push('"');
    } else {
      out.push_str(line);
    }
  });
  // to update Cargo.lock
  run(Command::new("cargo").arg("build"));
  run(Command::new("git").arg("add").args([
    "Cargo.lock",
    "Cargo.toml",
    "docs/CHANGELOG.md",
    "editors/vscode/package-lock.json",
    "editors/vscode/package.json",
  ]));
  let msg = format!("Release {tag_arg}");
  run(Command::new("git").args(["commit", "-m", msg.as_str(), "--no-verify"]));
  run(Command::new("git").arg("tag").arg(tag_arg));
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
      let dist_args = DistArgs {
        release: args.contains("--release"),
        with_cli: args.contains("--with-cli"),
        target: args.opt_value_from_str("--target").expect("no parse"),
      };
      finish_args(args);
      dist(&dist_args);
    }
    Cmd::Release => {
      let tag_arg: String = args.opt_value_from_str("--tag").expect("no parse").expect("no --tag");
      finish_args(args);
      release(tag_arg.as_str());
      run_ci();
    }
  }
}
