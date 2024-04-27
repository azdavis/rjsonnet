//! Analyze files from the CLI once, then exit.

#![allow(clippy::disallowed_methods)]

use lang_srv_state::State as _;
use pico_args::Arguments;
use std::process::ExitCode;

fn run() -> usize {
  let logger_env = env_logger::Env::default();
  env_logger::try_init_from_env(logger_env).expect("init logger");

  let pwd = std::env::current_dir().expect("get current dir");
  let clean_pwd = paths::CleanPathBuf::new(&pwd).expect("current dir not absolute");

  let mut args = Arguments::from_env();
  let name_only = args.contains("--name-only");
  let manifest = args.contains("--manifest");
  let show_diagnostics = args
    .opt_value_from_str::<_, jsonnet_analyze::ShowDiagnostics>("--show-diagnostics")
    .expect("show diagnostics")
    .unwrap_or_default();
  let max_diagnostics_per_file = args
    .opt_value_from_str::<_, jsonnet_analyze::DefaultUsize<5>>("--max-diagnostics-per-file")
    .expect("diagnostics per file")
    .unwrap_or_default();
  let mut root_dirs = vec![clean_pwd.clone()];
  while let Some(root) = args.opt_value_from_str::<_, String>("--root-dir").expect("parse arg") {
    root_dirs.push(clean_pwd.as_clean_path().join(root.as_str()));
  }
  let files = args.finish();

  let mut ret = 0usize;
  let init = jsonnet_analyze::Init {
    relative_to: Some(clean_pwd.clone()),
    manifest,
    root_dirs,
    show_diagnostics,
    max_diagnostics_per_file,
  };
  let mut st = jsonnet_analyze::St::init(init);
  let fs = paths::RealFileSystem::default();

  for arg in files {
    let p = clean_pwd.as_clean_path().join(arg.as_os_str());

    let ds_map = st.update_many(&fs, Vec::new(), vec![p.clone()]);

    if manifest {
      let path_id = st.path_id(p);
      match st.get_json(path_id) {
        Ok(json) => {
          let json = json.display(st.strings());
          println!("{json}");
        }
        Err(e) => {
          let e = e.display(st.strings(), st.paths(), Some(clean_pwd.as_clean_path()));
          println!("error: {e}");
          ret += 1;
        }
      }
    }

    for (path, ds) in ds_map {
      let path = st.paths().get_path(path).as_path();
      let path = path.strip_prefix(pwd.as_path()).unwrap_or(path);
      let path = path.display();

      ret += ds.len();

      if name_only {
        println!("{path}");
      } else {
        for d in ds {
          println!("{path}:{}: {}", d.range.start, d.message);
        }
      }
    }
  }

  ret
}

fn main() -> ExitCode {
  let num_errors = run();
  if num_errors == 0 {
    println!("no errors!");
    ExitCode::SUCCESS
  } else {
    let s = if num_errors == 1 { "" } else { "s" };
    println!("{num_errors} error{s}");
    ExitCode::FAILURE
  }
}
