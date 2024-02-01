//! Analyze jsonnet files.

use paths::{PathId, PathMap};
use rayon::iter::{
  IndexedParallelIterator as _, IntoParallelIterator as _, IntoParallelRefIterator,
  ParallelIterator as _,
};
use std::collections::{BTreeMap, BTreeSet};
use std::path::{Path, PathBuf};

/// The state of analysis.
#[derive(Debug, Default)]
pub struct St {
  artifacts: jsonnet_expr::Artifacts,
  files: PathMap<jsonnet_eval::JsonnetFile>,
  json: PathMap<jsonnet_eval::error::Result<jsonnet_eval::Json>>,
  /// a map from path ids *p* to the set of path ids that **depend on** *p*
  dependents: BTreeMap<PathId, BTreeSet<PathId>>,
}

impl St {
  /// Updates the state with added and removed Jsonnet paths.
  ///
  /// # Panics
  ///
  /// If the paths couldn't be read, or weren't file paths, or couldn't canonicalize, or if they had
  /// any jsonnet errors.
  pub fn update_many<F>(&mut self, fs: &F, remove: Vec<PathBuf>, add: Vec<PathBuf>)
  where
    F: Sync + Send + paths::FileSystem,
  {
    // first remove files, and start keeping track of what remaining files were updated.
    let updated = remove.into_iter().flat_map(|path| {
      let path_id = self.path_id(fs, path.as_path());
      self.files.remove(&path_id);
      let dependents = self.dependents.remove(&path_id);
      dependents.into_iter().flatten()
    });
    let mut updated: BTreeSet<_> = updated.collect();

    // get the file artifacts for each added file in parallel.
    let get_file_artifacts = add.into_par_iter().map(|path| {
      let contents = fs.read_to_string(path.as_path()).expect("read to string");
      let parent = path.parent().expect("no parent");
      let artifacts = FileArtifacts::new(contents.as_str(), parent, &FsAdapter(fs));
      (path, artifacts)
    });
    let mut file_artifacts = Vec::<(PathBuf, FileArtifacts)>::default();
    get_file_artifacts.collect_into_vec(&mut file_artifacts);

    // combine the file artifacts in sequence, and note which files were added.
    let added = file_artifacts.into_iter().map(|(path, artifacts)| {
      artifacts.panic_if_any_errors();
      let mut file = jsonnet_eval::JsonnetFile {
        expr_ar: artifacts.desugar.arenas.expr,
        top: artifacts.desugar.top,
      };
      let artifacts = jsonnet_expr::Artifacts {
        paths: artifacts.desugar.ps,
        strings: artifacts.desugar.arenas.str,
      };
      jsonnet_expr::combine::get(&mut self.artifacts, artifacts, &mut file.expr_ar);
      let path_id = self.path_id(fs, path.as_path());
      self.files.insert(path_id, file);
      path_id
    });
    let added: BTreeSet<_> = added.collect();

    // compute a mapping from path id p to non-empty set of path ids S, s.t. for all q in S, q was
    // just added, and p depends on q.
    let added_deps = self.files.par_iter().filter_map(|(&path_id, file)| {
      let deps: BTreeSet<_> = file
        .expr_ar
        .iter()
        .filter_map(|(_, expr)| {
          if let jsonnet_expr::ExprData::Import { path, .. } = *expr {
            added.contains(&path).then_some(path)
          } else {
            None
          }
        })
        .collect();
      if deps.is_empty() {
        None
      } else {
        Some((path_id, deps))
      }
    });
    let added_deps: PathMap<_> = added_deps.collect();

    // using that mapping, update the dependents.
    for (&path, deps) in &added_deps {
      for &dep in deps {
        self.dependents.entry(dep).or_default().insert(path);
      }
    }

    // added files were updated.
    updated.extend(added);

    // repeatedly update files until they don't need updating anymore.
    while !updated.is_empty() {
      let cx = self.cx();
      // manifest in parallel for all updated files.
      let updated_vals = updated.par_iter().map(|&path| {
        let val = jsonnet_eval::get_exec(cx, path);
        let val = val.and_then(|val| jsonnet_eval::get_manifest(cx, val));
        (path, val)
      });
      let updated_vals: PathMap<_> = updated_vals.collect();
      updated.clear();
      for (path_id, json) in updated_vals {
        // TODO could check if new json == old json and not add dependents if same
        self.json.insert(path_id, json);
        let dependents = self.dependents.get(&path_id);
        updated.extend(dependents.into_iter().flatten());
      }
    }
  }

  /// Return an evaluation context from this.
  fn cx(&self) -> jsonnet_eval::Cx<'_> {
    jsonnet_eval::Cx {
      jsonnet_files: &self.files,
      paths: &self.artifacts.paths,
      str_ar: &self.artifacts.strings,
    }
  }

  /// Returns a path id for this path.
  ///
  /// # Panics
  ///
  /// If we could not canonicalize the path.
  pub fn path_id<F>(&mut self, fs: &F, path: &Path) -> PathId
  where
    F: paths::FileSystem,
  {
    let canonical = fs.canonicalize(path).expect("canonicalize");
    self.artifacts.paths.get_id_owned(canonical)
  }

  /// Returns the string arena for this.
  #[must_use]
  pub fn strings(&self) -> &jsonnet_expr::StrArena {
    &self.artifacts.strings
  }

  /// Returns the mutable string arena for this.
  pub fn strings_mut(&mut self) -> &mut jsonnet_expr::StrArena {
    &mut self.artifacts.strings
  }

  /// Returns the mutable paths store for this.
  pub fn paths_mut(&mut self) -> &mut paths::Store {
    &mut self.artifacts.paths
  }

  /// Returns the json for this path.
  ///
  /// # Errors
  ///
  /// If this path couldn't be evaluated to json.
  ///
  /// # Panics
  ///
  /// If there was no json for this path.
  pub fn get_json(&self, path_id: PathId) -> &jsonnet_eval::error::Result<jsonnet_eval::Json> {
    self.json.get(&path_id).expect("get json")
  }
}

/// Artifacts from a file analyzed in isolation.
#[derive(Debug)]
struct FileArtifacts {
  lex_errors: Vec<jsonnet_lex::Error>,
  parse: jsonnet_parse::Parse,
  desugar: jsonnet_desugar::Desugar,
  statics_errors: Vec<jsonnet_statics::error::Error>,
}

impl FileArtifacts {
  /// Returns artifacts for a file contained in the given directory.
  fn new(contents: &str, current_dir: &Path, fs: &dyn jsonnet_desugar::FileSystem) -> Self {
    let lex = jsonnet_lex::get(contents);
    let parse = jsonnet_parse::get(&lex.tokens);
    let desugar = jsonnet_desugar::get(current_dir, &[], fs, parse.root.clone().into_ast());
    let statics_errors = jsonnet_statics::get(&desugar.arenas, desugar.top);
    Self { lex_errors: lex.errors, parse, desugar, statics_errors }
  }

  fn panic_if_any_errors(&self) {
    if let Some(e) = self.lex_errors.first() {
      panic!("lex error: {e}");
    }
    if let Some(e) = self.parse.errors.first() {
      panic!("parse error: {e}");
    }
    if let Some(e) = self.desugar.errors.first() {
      panic!("desugar error: {e}");
    }
    if let Some(e) = self.statics_errors.first() {
      let e = e.display(&self.desugar.arenas.str);
      panic!("statics error: {e}");
    }
  }
}

/// An adaptor between file system traits.
struct FsAdapter<'a, F>(&'a F);

impl<'a, F> jsonnet_desugar::FileSystem for FsAdapter<'a, F>
where
  F: paths::FileSystem,
{
  fn canonicalize(&self, p: &Path) -> std::io::Result<paths::CanonicalPathBuf> {
    self.0.canonicalize(p)
  }
}
