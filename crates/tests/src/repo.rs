//! Tests about the repository itself.

use rustc_hash::{FxHashMap, FxHashSet};
use std::collections::hash_map::Entry;
use std::collections::BTreeSet;
use std::{env, fs, path::Path, process::Command};

fn eq_sets<T>(lhs: &BTreeSet<T>, rhs: &BTreeSet<T>, only_lhs: &str, only_rhs: &str)
where
  T: Ord + std::fmt::Debug,
{
  let only_lhs_set: Vec<_> = lhs.difference(rhs).collect();
  assert!(only_lhs_set.is_empty(), "{only_lhs}: {only_lhs_set:#?}");
  let only_rhs_set: Vec<_> = rhs.difference(lhs).collect();
  assert!(only_rhs_set.is_empty(), "{only_rhs}: {only_rhs_set:#?}");
}

fn empty_set<T>(set: &BTreeSet<T>, msg: &str)
where
  T: Ord + std::fmt::Debug,
{
  assert!(set.is_empty(), "{msg}: {set:#?}");
}

fn root_dir() -> &'static Path {
  Path::new(env!("CARGO_MANIFEST_DIR")).parent().unwrap().parent().unwrap()
}

fn root_cmd(s: &str) -> Command {
  let mut ret = Command::new(s);
  ret.current_dir(root_dir());
  ret
}

fn output(c: &mut Command) -> String {
  let out = c.output().unwrap();
  assert!(out.status.success());
  String::from_utf8(out.stdout).unwrap()
}

fn empty_git_grep(patterns: &[&str], msg: &str) {
  let mut cmd = root_cmd("git");
  cmd.args(["grep", "-Fn"]);
  for pattern in patterns {
    cmd.args(["-e", pattern]);
  }
  let out = cmd.output().unwrap();
  assert!(out.stderr.is_empty());
  let out = String::from_utf8(out.stdout).unwrap();
  let lines: Vec<_> = out.lines().collect();
  assert!(lines.is_empty(), "{msg}: {lines:#?}");
}

#[test]
fn architecture() {
  let in_doc: BTreeSet<_> = include_str!("../../../docs/ARCHITECTURE.md")
    .lines()
    .filter_map(|line| Some(line.strip_prefix("### `")?.strip_suffix('`')?.to_owned()))
    .collect();
  let mut no_doc = BTreeSet::from(["crates", "README.md"]);
  let on_fs: BTreeSet<_> = std::iter::empty()
    .chain(fs::read_dir(root_dir().join("crates")).unwrap().filter_map(|x| {
      let file_name = x.ok()?.file_name();
      let file_name = file_name.to_str()?;
      Some(format!("crates/{file_name}"))
    }))
    .chain(fs::read_dir(root_dir().join("editors")).unwrap().filter_map(|x| {
      let file_name = x.ok()?.file_name();
      let file_name = file_name.to_str()?;
      Some(format!("editors/{file_name}"))
    }))
    .chain(output(root_cmd("git").args(["ls-tree", "--name-only", "HEAD"])).lines().filter_map(
      |x| {
        if no_doc.remove(x) {
          None
        } else {
          Some(x.to_owned())
        }
      },
    ))
    .collect();
  empty_set(&no_doc, "explicitly non-documented items not found on fs");
  eq_sets(&in_doc, &on_fs, "documented items that don't exist", "items without documentation");
}

#[test]
fn no_debugging() {
  // the uppercase + to_ascii_lowercase is to prevent git grep from triggering on this file.
  let fst = "DBG".to_ascii_lowercase();
  let snd = "EPRINT".to_ascii_lowercase();
  let thd = "CONSOLE.LOG".to_ascii_lowercase();
  empty_git_grep(&[fst.as_str(), snd.as_str(), thd.as_str()], "files with debugging");
}

#[test]
fn licenses() {
  let allowed = [
    "(MIT OR Apache-2.0) AND Unicode-DFS-2016",
    "0BSD OR MIT OR Apache-2.0",
    "Apache-2.0 OR BSL-1.0",
    "Apache-2.0 OR MIT",
    "Apache-2.0/MIT",
    "CC0-1.0 OR MIT-0 OR Apache-2.0",
    "MIT OR Apache-2.0 OR Zlib",
    "MIT OR Apache-2.0",
    "MIT OR Zlib OR Apache-2.0",
    "MIT",
    "MIT/Apache-2.0",
    "Unlicense OR MIT",
    "Unlicense/MIT",
    "Zlib OR Apache-2.0 OR MIT",
  ];
  let mut allowed: FxHashMap<_, _> = allowed.iter().map(|&x| (x, false)).collect();
  let out = output(root_cmd("cargo").args(["metadata", "--format-version", "1"]));
  let metadata: serde_json::Map<String, serde_json::Value> = serde_json::from_str(&out).unwrap();
  let packages = metadata.get("packages").unwrap().as_array().unwrap();
  let mut new_licenses = FxHashMap::<&str, FxHashSet<&str>>::default();
  for package in packages {
    let package = package.as_object().unwrap();
    let license = package.get("license").unwrap().as_str().unwrap();
    match allowed.entry(license) {
      Entry::Occupied(mut entry) => {
        entry.insert(true);
        continue;
      }
      Entry::Vacant(_) => {}
    }
    let name = package.get("name").unwrap().as_str().unwrap();
    new_licenses.entry(license).or_default().insert(name);
  }
  for (license, names) in &new_licenses {
    println!("{license}: {names:?}");
  }
  assert!(new_licenses.is_empty(), "found {} new licenses", new_licenses.len());
  let allowed_unused: BTreeSet<_> =
    allowed.into_iter().filter_map(|(k, used)| (!used).then_some(k)).collect();
  empty_set(&allowed_unused, "allowed but unused license");
}

#[test]
fn rs_file_comments() {
  let out = output(root_cmd("git").args(["ls-files", "**/*.rs"]));
  let no_doc: BTreeSet<_> = out
    .lines()
    .filter(|file| {
      let out = fs::read_to_string(root_dir().join(file)).unwrap();
      let fst = out.lines().next().unwrap();
      !fst.starts_with("//! ")
    })
    .collect();
  empty_set(&no_doc, "rust files without doc comment at top");
}

#[test]
fn cargo_toml() {
  for entry in std::fs::read_dir(root_dir().join("crates")).unwrap() {
    let entry = entry.unwrap();
    let mut path = entry.path();
    path.push("Cargo.toml");
    let contents = std::fs::read_to_string(&path).unwrap();
    let mut lines = contents.lines();
    if !lines.by_ref().any(|line| matches!(line, "[dependencies]" | "[dev-dependencies]")) {
      continue;
    }
    let mut workspace = true;
    let mut ps = Vec::<&str>::new();
    for line in lines {
      if line.starts_with('[') || line.ends_with("# @ignore") {
        break;
      }
      if line.is_empty() {
        workspace = false;
        continue;
      }
      let (lhs, rhs) = line.split_once(" = ").unwrap();
      let (pkg, dot_what) = lhs.split_once('.').expect(lhs);
      match dot_what {
        "path" => {
          let mut ps_sorted = ps.clone();
          ps_sorted.sort_unstable();
          assert_eq!(ps, ps_sorted);
          ps.clear();
          workspace = false;
          let basename = rhs.strip_prefix("\"../").unwrap().strip_suffix('\"').unwrap();
          assert_eq!(pkg, basename);
        }
        "workspace" => {
          assert!(workspace);
          assert_eq!(rhs, "true");
        }
        _ => unreachable!("unknown dot_what: {dot_what}"),
      }
      ps.push(pkg);
    }
    let mut ps_sorted = ps.clone();
    ps_sorted.sort_unstable();
    assert_eq!(ps, ps_sorted);
  }
}
