//! Analyze jsonnet files.

#![allow(clippy::too_many_lines)]

use always::always;
use diagnostic::Diagnostic;
use paths::{PathId, PathMap};
use rayon::iter::{IntoParallelIterator as _, IntoParallelRefIterator, ParallelIterator as _};
use std::collections::{BTreeMap, BTreeSet};
use std::path::{Path, PathBuf};

/// The state of analysis.
#[derive(Debug, Default)]
pub struct St {
  artifacts: jsonnet_expr::Artifacts,
  files: PathMap<jsonnet_eval::JsonnetFile>,
  files_extra: PathMap<FileArtifacts>,
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
  pub fn update_many<F>(
    &mut self,
    fs: &F,
    remove: Vec<PathBuf>,
    add: Vec<PathBuf>,
  ) -> PathMap<Vec<Diagnostic>>
  where
    F: Sync + Send + paths::FileSystem,
  {
    // first remove files, and start keeping track of what remaining files were updated.
    //
    // NOTE: for each r in remove, we DO NOT bother removing r from s for any (_, s) in dependents.
    let updated = remove.into_iter().filter_map(|path| {
      let path = match fs.canonicalize(path.as_path()) {
        Ok(x) => x,
        Err(e) => {
          always!(false, "canonicalize error: {e}");
          return None;
        }
      };
      let path_id = self.path_id(path);
      self.files.remove(&path_id);
      self.files_extra.remove(&path_id);
      self.dependents.remove(&path_id)
    });
    let mut updated: BTreeSet<_> = updated.flatten().collect();

    // get the file artifacts for each added file in parallel.
    let get_file_artifacts = add.into_par_iter().filter_map(|path| {
      let contents = match fs.read_to_string(path.as_path()) {
        Ok(x) => x,
        Err(e) => {
          always!(false, "read to string error: {e}");
          return None;
        }
      };
      let Some(parent) = path.parent() else {
        always!(false, "no parent: {}", path.display());
        return None;
      };
      let artifacts = IsolatedFileArtifacts::new(contents.as_str(), parent, &FsAdapter(fs));
      Some((path, artifacts))
    });
    let file_artifacts: Vec<_> = get_file_artifacts.collect();

    // combine the file artifacts in sequence, and note which files were added.
    let added = file_artifacts.into_iter().filter_map(|(path, mut art)| {
      let subst = jsonnet_expr::Subst::get(&mut self.artifacts, art.combine);
      for (_, ed) in art.eval.expr_ar.iter_mut() {
        ed.apply(&subst);
      }
      for err in &mut art.extra.errors.statics {
        err.apply(&subst);
      }
      let path = match fs.canonicalize(path.as_path()) {
        Ok(x) => x,
        Err(e) => {
          always!(false, "canonicalize error: {e}");
          return None;
        }
      };
      let path_id = self.path_id(path);
      self.files.insert(path_id, art.eval);
      self.files_extra.insert(path_id, art.extra);
      Some(path_id)
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
    let mut ret = PathMap::<Vec<Diagnostic>>::default();
    while !updated.is_empty() {
      let cx = self.cx();
      // manifest in parallel for all updated files. TODO this probably doesn't respect ordering
      // requirements among the updated. what if one updated file depends on another updated file?
      let updated_vals = updated.par_iter().flat_map(|&path| {
        // only exec if no errors in the file so far.
        self.files_extra[&path].errors.is_empty().then(|| {
          let val = jsonnet_eval::get_exec(cx, path);
          let val = val.and_then(|val| jsonnet_eval::get_manifest(cx, val));
          (path, val)
        })
      });
      let updated_vals: PathMap<_> = updated_vals.collect();
      let iter = updated.iter().map(|&path| {
        let art = &self.files_extra[&path];
        let ds: Vec<_> = std::iter::empty()
          .chain(art.errors.lex.iter().filter_map(|err| {
            let range = err.range();
            let Some(range) = art.pos_db.range_utf16(range) else {
              always!(false, "bad range: {range:?}");
              return None;
            };
            Some(Diagnostic { range, message: err.to_string() })
          }))
          .chain(art.errors.parse.iter().filter_map(|err| {
            let range = err.range();
            let Some(range) = art.pos_db.range_utf16(range) else {
              always!(false, "bad range: {range:?}");
              return None;
            };
            Some(Diagnostic { range, message: err.to_string() })
          }))
          .chain(art.errors.desugar.iter().filter_map(|err| {
            let range = err.range();
            let Some(range) = art.pos_db.range_utf16(range) else {
              always!(false, "bad range: {range:?}");
              return None;
            };
            Some(Diagnostic { range, message: err.to_string() })
          }))
          .chain(art.errors.statics.iter().filter_map(|err| {
            let expr = err.expr();
            let ptr = art.pointers.get_ptr(expr);
            let range = ptr.text_range();
            let Some(range) = art.pos_db.range_utf16(range) else {
              always!(false, "bad range: {range:?}");
              return None;
            };
            let err = err.display(&self.artifacts.strings);
            Some(Diagnostic { range, message: err.to_string() })
          }))
          .collect();
        (path, ds)
      });
      ret.extend(iter);
      updated.clear();
      for (path_id, json) in updated_vals {
        // TODO could check if new json == old json and not add dependents if same
        self.json.insert(path_id, json);
        // TODO doesn't skip dependents that were already updated
        let dependents = self.dependents.get(&path_id);
        updated.extend(dependents.into_iter().flatten());
      }
    }
    ret
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
  pub fn path_id(&mut self, path: paths::CanonicalPathBuf) -> PathId {
    self.artifacts.paths.get_id_owned(path)
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

  /// Returns the paths store for this.
  #[must_use]
  pub fn paths(&self) -> &paths::Store {
    &self.artifacts.paths
  }

  /// Returns the json for this path.
  ///
  /// # Errors
  ///
  /// If this path couldn't be evaluated to json.
  pub fn get_json(&self, path_id: PathId) -> jsonnet_eval::error::Result<&jsonnet_eval::Json> {
    match self.json.get(&path_id) {
      Some(x) => match x {
        Ok(x) => Ok(x),
        Err(e) => Err(e.clone()),
      },
      None => Err(jsonnet_eval::error::Error::NoPath(path_id)),
    }
  }
}

/// Artifacts from a file whose shared artifacts have been combined into the global ones.
#[derive(Debug)]
#[allow(dead_code)]
struct FileArtifacts {
  pos_db: text_pos::PositionDb,
  syntax: jsonnet_syntax::Root,
  pointers: jsonnet_desugar::Pointers,
  errors: FileErrors,
}

/// Errors from a file analyzed in isolation.
#[derive(Debug)]
#[allow(dead_code)]
struct FileErrors {
  lex: Vec<jsonnet_lex::Error>,
  parse: Vec<jsonnet_parse::Error>,
  desugar: Vec<jsonnet_desugar::Error>,
  statics: Vec<jsonnet_statics::error::Error>,
}

impl FileErrors {
  fn is_empty(&self) -> bool {
    self.lex.is_empty()
      && self.parse.is_empty()
      && self.desugar.is_empty()
      && self.statics.is_empty()
  }
}

/// Artifacts from a file analyzed in isolation.
#[derive(Debug)]
struct IsolatedFileArtifacts {
  eval: jsonnet_eval::JsonnetFile,
  combine: jsonnet_expr::Artifacts,
  extra: FileArtifacts,
}

impl IsolatedFileArtifacts {
  /// Returns artifacts for a file contained in the given directory.
  fn new(contents: &str, current_dir: &Path, fs: &dyn jsonnet_desugar::FileSystem) -> Self {
    let lex = jsonnet_lex::get(contents);
    let parse = jsonnet_parse::get(&lex.tokens);
    let desugar = jsonnet_desugar::get(current_dir, &[], fs, parse.root.clone().expr());
    let statics_errors = jsonnet_statics::get(&desugar.arenas, desugar.top);
    Self {
      eval: jsonnet_eval::JsonnetFile { expr_ar: desugar.arenas.expr, top: desugar.top },
      combine: jsonnet_expr::Artifacts { paths: desugar.ps, strings: desugar.arenas.str },
      extra: FileArtifacts {
        pos_db: text_pos::PositionDb::new(contents),
        syntax: parse.root,
        pointers: desugar.pointers,
        errors: FileErrors {
          lex: lex.errors,
          parse: parse.errors,
          desugar: desugar.errors,
          statics: statics_errors,
        },
      },
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
