//! Formatting with jsonnetfmt.

use crate::util::FormatEngine;
use paths::{CleanPath, CleanPathBuf};
use std::fmt;
use std::io::{self, Write as _};
use std::process::{Command, Stdio};

const DIR: &str = "bin";
const PROG: &str = "jsonnetfmt";

pub(crate) fn get(
  engine: FormatEngine,
  mut root: CleanPathBuf,
  path: &CleanPath,
  contents: &str,
) -> Result<String, Error> {
  match engine {
    FormatEngine::BinJsonnetFmtStdio => {}
  }
  root.push(DIR);
  root.push(PROG);
  let mut prog = Command::new(root.as_path())
    .arg("-stdio")
    .arg(path.as_path())
    .stdin(Stdio::piped())
    .stdout(Stdio::piped())
    .spawn()
    .map_err(Error::Spawn)?;
  let mut stdin = prog.stdin.take().ok_or(Error::TakeStdin)?;
  stdin.write_all(contents.as_bytes()).map_err(Error::WriteAll)?;
  // explicitly drop to close
  drop(stdin);
  let out = prog.wait_with_output().map_err(Error::Wait)?;
  if !out.status.success() {
    return Err(Error::Unsuccessful(out.stderr));
  }
  String::from_utf8(out.stdout).map_err(Error::StdoutUtf8)
}

#[derive(Debug)]
pub(crate) enum Error {
  Spawn(io::Error),
  TakeStdin,
  WriteAll(io::Error),
  Wait(io::Error),
  Unsuccessful(Vec<u8>),
  StdoutUtf8(std::string::FromUtf8Error),
}

impl fmt::Display for Error {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Error::Spawn(e) => write!(f, "couldn't spawn `{DIR}/{PROG}`: {e}"),
      Error::TakeStdin => write!(f, "couldn't take stdin of `{DIR}/{PROG}`"),
      Error::WriteAll(e) => write!(f, "couldn't write stdin of `{DIR}/{PROG}`: {e}"),
      Error::Wait(e) => write!(f, "couldn't wait for `{DIR}/{PROG}`: {e}"),
      Error::Unsuccessful(stderr) => {
        let stderr = String::from_utf8_lossy(stderr.as_slice());
        let len = stderr.len();
        write!(f, "didn't get successful exit from `{DIR}/{PROG}`; stderr (len {len}): {stderr}")
      }
      Error::StdoutUtf8(e) => write!(f, "couldn't convert stdout of `{DIR}/{PROG}` to UTF-8: {e}"),
    }
  }
}

impl std::error::Error for Error {
  fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
    match self {
      Error::TakeStdin | Error::Unsuccessful(_) => None,
      Error::Spawn(e) | Error::WriteAll(e) | Error::Wait(e) => Some(e),
      Error::StdoutUtf8(e) => Some(e),
    }
  }
}
