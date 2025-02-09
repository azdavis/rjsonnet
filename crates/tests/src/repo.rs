//! Tests about the repository itself.

use rustc_hash::{FxHashMap, FxHashSet};
use std::collections::hash_map::Entry;
use std::collections::BTreeSet;
use std::fmt::{self, Write as _};
use std::sync::LazyLock;
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
  let mut no_doc = BTreeSet::from(["crates", "LICENSE-APACHE.md", "LICENSE-MIT.md", "README.md"]);
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
fn changelog() {
  if std::env::var_os("CI").is_some_and(|x| x == "1") {
    return;
  }
  let tag_out = output(root_cmd("git").arg("tag"));
  let in_git: BTreeSet<_> = tag_out.lines().filter(|x| x.starts_with('v')).collect();
  let in_doc: BTreeSet<_> = include_str!("../../../docs/CHANGELOG.md")
    .lines()
    .filter_map(|line| {
      let title = line.strip_prefix("## ")?;
      // allow a title of 'main' for unreleased changes.
      (title != "main").then_some(title)
    })
    .collect();
  eq_sets(&in_git, &in_doc, "tags that have no changelog entry", "changelog entries without a tag");
}

static METADATA: LazyLock<serde_json::Map<String, serde_json::Value>> = LazyLock::new(|| {
  let out = output(root_cmd("cargo").args(["metadata", "--format-version", "1"]));
  serde_json::from_str(&out).unwrap()
});

#[test]
fn licenses() {
  let allowed = [
    "(MIT OR Apache-2.0) AND Unicode-3.0",
    "0BSD OR MIT OR Apache-2.0",
    "Apache-2.0 OR BSL-1.0",
    "Apache-2.0 OR MIT",
    "Apache-2.0/MIT",
    "MIT OR Apache-2.0",
    "MIT OR Zlib OR Apache-2.0",
    "MIT",
    "MIT/Apache-2.0",
    "Unicode-3.0",
    "Unlicense OR MIT",
  ];
  let mut allowed: FxHashMap<_, _> = allowed.iter().map(|&x| (x, false)).collect();
  let packages = METADATA.get("packages").unwrap().as_array().unwrap();
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

#[derive(Debug)]
struct EnumVariant<'a> {
  name: &'a str,
  desc: String,
}

impl fmt::Display for EnumVariant<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "`{:?}`: {}", self.name, self.desc)
  }
}

#[derive(Debug)]
enum TypeAndDefault<'a> {
  String(&'a str, Vec<EnumVariant<'a>>),
  Bool(bool),
  Number(u64),
  Array(&'a [serde_json::Value]),
}

impl fmt::Display for TypeAndDefault<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      TypeAndDefault::String(s, vs) => {
        writeln!(f, "- Type: `string`")?;
        writeln!(f, "- Default: `{s:?}`")?;
        if !vs.is_empty() {
          writeln!(f, "- Valid values:")?;
          for v in vs {
            writeln!(f, "  - {v}")?;
          }
        }
        Ok(())
      }
      TypeAndDefault::Bool(b) => {
        writeln!(f, "- Type: `boolean`")?;
        writeln!(f, "- Default: `{b}`")?;
        Ok(())
      }
      TypeAndDefault::Number(n) => {
        writeln!(f, "- Type: `number`")?;
        writeln!(f, "- Default: `{n}`")?;
        Ok(())
      }
      TypeAndDefault::Array(ar) => {
        writeln!(f, "- Type: `array`")?;
        write!(f, "- Default: `[")?;
        let mut iter = ar.iter();
        if let Some(elem) = iter.next() {
          write!(f, "{elem}")?;
        }
        for elem in iter {
          write!(f, ", {elem}")?;
        }
        writeln!(f, "]`")?;
        Ok(())
      }
    }
  }
}

#[derive(Debug)]
struct ConfigProperty<'a> {
  name: &'a str,
  desc: &'a str,
  type_and_default: TypeAndDefault<'a>,
}

impl fmt::Display for ConfigProperty<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    writeln!(f, "#### `{}`", self.name)?;
    writeln!(f)?;
    self.type_and_default.fmt(f)?;
    writeln!(f)?;
    writeln!(f, "{}", self.desc)?;
    Ok(())
  }
}

const PACKAGE_JSON: &str = include_str!("../../../editors/vscode/package.json");
const PACKAGE_LOCK_JSON: &str = include_str!("../../../editors/vscode/package-lock.json");
const MANUAL: &str = include_str!("../../../docs/manual.md");
const COMMENT_CLOSE: &str = " -->";

fn is_section_comment(s: &str, comment_open: &str, section: &str) -> bool {
  s.trim().strip_prefix(comment_open).and_then(|s| s.strip_suffix(COMMENT_CLOSE)) == Some(section)
}

fn get_manual_section(section: &str) -> impl Iterator<Item = &'static str> + use<'_> {
  let mut iter = MANUAL.lines();
  iter.find(|s| is_section_comment(s, "<!-- @begin ", section));
  iter.take_while(|s| !is_section_comment(s, "<!-- @end ", section))
}

#[test]
fn vs_code_config() {
  let package_json: serde_json::Value = serde_json::from_str(PACKAGE_JSON).unwrap();
  let properties = package_json
    .as_object()
    .unwrap()
    .get("contributes")
    .unwrap()
    .as_object()
    .unwrap()
    .get("configuration")
    .unwrap()
    .as_object()
    .unwrap()
    .get("properties")
    .unwrap()
    .as_object()
    .unwrap();
  let mut want_doc = String::new();
  for (name, val) in properties {
    let val = val.as_object().unwrap();
    let typ = val.get("type").unwrap().as_str().unwrap();
    let default_ = val.get("default").unwrap();
    let desc = val.get("markdownDescription").unwrap().as_str().unwrap();
    let enums = val.get("enum").and_then(serde_json::Value::as_array);
    let enum_descs = val.get("markdownEnumDescriptions").and_then(serde_json::Value::as_array);
    assert_eq!(enums.map(Vec::len), enum_descs.map(Vec::len));
    let enum_variants: Vec<_> = enums
      .into_iter()
      .flatten()
      .zip(enum_descs.into_iter().flatten())
      .map(|(name, desc)| EnumVariant {
        name: name.as_str().unwrap(),
        // HACK: indent unordered list items
        desc: desc.as_str().unwrap().replace("\n- ", "\n    - "),
      })
      .collect();
    let type_and_default = match typ {
      "string" => TypeAndDefault::String(default_.as_str().unwrap(), enum_variants),
      "boolean" => {
        assert!(enum_variants.is_empty());
        TypeAndDefault::Bool(default_.as_bool().unwrap())
      }
      "number" => {
        assert!(enum_variants.is_empty());
        TypeAndDefault::Number(default_.as_number().unwrap().as_u64().unwrap())
      }
      "array" => {
        assert!(enum_variants.is_empty());
        TypeAndDefault::Array(default_.as_array().unwrap())
      }
      _ => panic!("unknown type: {typ}"),
    };
    let config_property = ConfigProperty { name, desc, type_and_default };
    writeln!(want_doc, "{config_property}").unwrap();
  }
  let got_doc_lines: Vec<_> = get_manual_section("vscode-config").collect();
  let got_doc = got_doc_lines.join("\n");
  pretty_assertions::assert_str_eq!(got_doc.trim(), want_doc.trim(),);
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
fn version() {
  let package_json: serde_json::Value = serde_json::from_str(PACKAGE_JSON).unwrap();
  let package_lock_json: serde_json::Value = serde_json::from_str(PACKAGE_LOCK_JSON).unwrap();
  let package_lock_json = package_lock_json.as_object().unwrap();
  let cargo_toml = include_str!("../../../Cargo.toml");
  let pkg_json_ver = package_json.as_object().unwrap().get("version").unwrap().as_str().unwrap();
  let pkg_lk_json_ver = package_lock_json.get("version").unwrap().as_str().unwrap();
  let pkg_lk_json_self_ver = package_lock_json
    .get("packages")
    .unwrap()
    .as_object()
    .unwrap()
    .get("")
    .unwrap()
    .as_object()
    .unwrap()
    .get("version")
    .unwrap()
    .as_str()
    .unwrap();
  let cargo_toml_ver = cargo_toml
    .lines()
    .find_map(|line| line.strip_prefix("version = \"")?.strip_suffix('"'))
    .unwrap();

  assert_eq!(pkg_json_ver, pkg_lk_json_ver);
  assert_eq!(pkg_json_ver, pkg_lk_json_self_ver);
  assert_eq!(pkg_json_ver, cargo_toml_ver);

  for member in METADATA.get("workspace_members").unwrap().as_array().unwrap() {
    let member = member.as_str().unwrap();
    let (_, member_version) = member.split_once('#').unwrap();
    assert_eq!(pkg_json_ver, member_version);
  }

  if let Some(github_ref_v) = option_env!("GITHUB_REF_NAME").and_then(|r| r.strip_prefix('v')) {
    assert_eq!(pkg_json_ver, github_ref_v);
  }
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
        _ => panic!("unknown dot_what: {dot_what}"),
      }
      ps.push(pkg);
    }
    let mut ps_sorted = ps.clone();
    ps_sorted.sort_unstable();
    assert_eq!(ps, ps_sorted);
  }
}
