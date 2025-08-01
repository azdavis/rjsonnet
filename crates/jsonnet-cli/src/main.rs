//! A CLI for static analysis.

use lang_srv_state::State as _;
use paths::FileSystem as _;
use std::{fmt, process::ExitCode};

fn main() -> ExitCode {
  let args = match get_args() {
    Err(e) => {
      println!("error parsing args: {e}");
      return ExitCode::FAILURE;
    }
    Ok(None) => return ExitCode::SUCCESS,
    Ok(Some(x)) => x,
  };
  let quiet = args.quiet;
  let n = run(args);
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

struct Args {
  rm_unused: Option<jsonnet_analyze::remove::Options>,
  quiet: bool,
  root_dirs: Option<String>,
  files: Vec<std::ffi::OsString>,
  allow_unused: AllowUnused,
}

fn get_args() -> Result<Option<Args>, pico_args::Error> {
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
    println!("  --rm-unused <flavor>");
    println!("    remove unused locals");
    println!("    <flavor> may be 'all' or 'imports'");
    println!("  --allow-unused <flavor>");
    println!("    when to allow unused locals");
    println!("    <flavor> may be 'never' or 'with-underscore'");
    println!("    defaults to 'with-underscore'");
    println!("    when 'with-underscore', locals that begin with an underscore");
    println!("    are not reported as unused");
    println!("    when 'never', unused locals are always reported as unused");
    println!("  --rm-unused-comments <flavor>");
    println!("    remove comments near unused items");
    println!("    only has effect with --rm-unused");
    println!("    <flavor> may be 'none', 'all', 'above', 'below'");
    println!("    defaults to 'none' if not specified");
    println!("  --root-dirs <dirs>");
    println!("    comma-separated extra root directories");
    println!();
    return Ok(None);
  }
  let rm_unused = args.opt_value_from_str::<_, jsonnet_analyze::remove::Flavor>("--rm-unused")?;
  let rm_comments = args.opt_value_from_str::<_, RmComments>("--rm-unused-comments")?;
  let allow_unused =
    args.opt_value_from_str::<_, AllowUnused>("--allow-unused")?.unwrap_or_default();
  let rm_unused = rm_unused.map(|flavor| jsonnet_analyze::remove::Options {
    flavor,
    comments: rm_comments.unwrap_or_default().into_analysis(),
  });
  let quiet = args.contains(["-q", "--quiet"]);
  let root_dirs: Option<String> = args.opt_value_from_str("--root-dirs")?;
  let files = args.finish();
  Ok(Some(Args { rm_unused, quiet, root_dirs, files, allow_unused }))
}

#[derive(Debug, Default, Clone, Copy)]
enum RmComments {
  #[default]
  None,
  All,
  Above,
  Below,
}

impl std::str::FromStr for RmComments {
  type Err = InvalidError;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    let ret = match s {
      "none" => Self::None,
      "all" => Self::All,
      "above" => Self::Above,
      "below" => Self::Below,
      _ => return Err(InvalidError),
    };
    Ok(ret)
  }
}

impl RmComments {
  fn into_analysis(self) -> jsonnet_analyze::remove::Comments {
    let (above, below) = match self {
      RmComments::None => (false, false),
      RmComments::All => (true, true),
      RmComments::Above => (true, false),
      RmComments::Below => (false, true),
    };
    jsonnet_analyze::remove::Comments { above, below }
  }
}

#[derive(Debug, Default, Clone, Copy)]
enum AllowUnused {
  Never,
  #[default]
  WithUnderscore,
}

impl std::str::FromStr for AllowUnused {
  type Err = InvalidError;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    let ret = match s {
      "never" => Self::Never,
      "with-underscore" => Self::WithUnderscore,
      _ => return Err(InvalidError),
    };
    Ok(ret)
  }
}

#[derive(Debug)]
struct InvalidError;

impl fmt::Display for InvalidError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.write_str("invalid option, see --help")
  }
}

fn run(args: Args) -> usize {
  let fs = paths::RealFileSystem::default();
  let pwd = match fs.current_dir() {
    Ok(x) => x,
    Err(e) => {
      if !args.quiet {
        println!("couldn't get current dir: {e}");
      }
      return 1;
    }
  };
  let root_dirs = args.root_dirs.iter().flat_map(|x| x.split(',')).map(|x| {
    let mut p = pwd.clone();
    p.push(x);
    p
  });
  let root_dirs: Vec<_> = root_dirs.collect();
  let init = jsonnet_analyze::Init {
    root_dirs,
    allow_unused_underscore: match args.allow_unused {
      AllowUnused::Never => false,
      AllowUnused::WithUnderscore => true,
    },
    ..Default::default()
  };
  let mut st = jsonnet_analyze::St::init(pwd.clone(), init);
  let mut ret = 0usize;
  for arg in args.files {
    let Some(arg) = arg.to_str() else {
      if !args.quiet {
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
        if !args.quiet {
          println!("{arg}: couldn't read path: {e}");
        }
        ret += 1;
        continue;
      }
    };
    let (_, ds) = st.open(&fs, p.clone(), contents);
    if let Some(options) = args.rm_unused {
      if let Some(contents) = st.remove_unused(&fs, p.as_clean_path(), options) {
        if let Err(e) = std::fs::write(p.as_path(), contents.as_bytes()) {
          if !args.quiet {
            println!("{arg}: couldn't write path: {e}");
          }
          ret += 1;
        }
      }
    }
    st.close(p);
    ret += ds.len();
    for d in ds {
      if !args.quiet {
        println!("{arg}:{d}");
      }
    }
  }
  ret
}
