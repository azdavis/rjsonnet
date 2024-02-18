//! Analyze jsonnet files.

#![allow(clippy::too_many_lines)]

use always::always;
use diagnostic::Diagnostic;
use paths::{PathId, PathMap};
use rayon::iter::{IntoParallelIterator as _, IntoParallelRefIterator as _, ParallelIterator as _};
use std::collections::{BTreeMap, BTreeSet};

/// The state of analysis.
#[derive(Debug, Default)]
pub struct St {
  artifacts: jsonnet_expr::Artifacts,
  files: PathMap<jsonnet_eval::JsonnetFile>,
  /// invariant: x in files iff x in files_extra.
  files_extra: PathMap<FileArtifacts>,
  /// invariant: if x in json, then x in files.
  json: PathMap<jsonnet_eval::error::Result<jsonnet_eval::Json>>,
  /// invariants:
  /// - if x in dependents, x in files.
  /// - if a depends on b, a in dependents[b]. (note: NOT a bi-implication)
  /// NON-invariants:
  /// - for all (_, s) in dependents, for all x in s, x in files.
  dependents: BTreeMap<PathId, BTreeSet<PathId>>,
}

impl St {
  /// Updates the state with added and removed Jsonnet paths.
  #[must_use]
  pub fn update_many<F>(
    &mut self,
    fs: &F,
    remove: Vec<paths::AbsPathBuf>,
    add: Vec<paths::AbsPathBuf>,
  ) -> PathMap<Vec<Diagnostic>>
  where
    F: Sync + Send + paths::FileSystem,
  {
    // first remove files, and start keeping track of what remaining files were updated.
    //
    // NOTE: for each r in remove, we DO NOT bother removing r from s for any (_, s) in dependents.
    let updated = remove.into_iter().filter_map(|path| {
      let path_id = self.path_id(path);
      self.files.remove(&path_id);
      self.files_extra.remove(&path_id);
      self.dependents.remove(&path_id)
    });
    let updated: BTreeSet<_> = updated.flatten().collect();

    // get the initial file artifacts in parallel.
    let file_artifacts = add.into_par_iter().filter_map(|path| {
      let mut art = get_isolated_fs(path.as_abs_path(), fs)?;
      let path = art.combine.paths.get_id(path.as_abs_path());
      Some((path, art))
    });
    let file_artifacts: Vec<_> = file_artifacts.collect();

    self.update(fs, updated, file_artifacts)
  }

  /// Updates one file.
  #[must_use]
  pub fn update_one<F>(
    &mut self,
    fs: &F,
    path: paths::AbsPathBuf,
    contents: &str,
  ) -> PathMap<Vec<Diagnostic>>
  where
    F: Sync + Send + paths::FileSystem,
  {
    let Some(art) = get_isolated_str(path.as_abs_path(), contents, fs) else {
      return PathMap::default();
    };
    let path = self.path_id(path);
    self.update(fs, BTreeSet::new(), vec![(path, art)])
  }

  fn update<F>(
    &mut self,
    fs: &F,
    mut updated: BTreeSet<paths::PathId>,
    mut to_add: Vec<(paths::PathId, IsolatedFileArtifacts)>,
  ) -> PathMap<Vec<Diagnostic>>
  where
    F: Sync + Send + paths::FileSystem,
  {
    let mut added = BTreeSet::<PathId>::new();

    // repeatedly add the new files and any relevant imports.
    while !to_add.is_empty() {
      // combine the file artifacts in sequence.
      let did_add = to_add.into_iter().map(|(mut path_id, mut art)| {
        let subst = jsonnet_expr::Subst::get(&mut self.artifacts, art.combine);

        for (_, ed) in art.eval.expr_ar.iter_mut() {
          ed.apply(&subst);
        }
        for err in &mut art.extra.errors.statics {
          err.apply(&subst);
        }
        path_id = subst.get_path_id(path_id);

        self.files.insert(path_id, art.eval);
        self.files_extra.insert(path_id, art.extra);
        added.insert(path_id);

        path_id
      });
      let did_add: BTreeSet<_> = did_add.collect();

      // the next set of things to add: imports from the added files that do not exist and were not
      // themselves already added.
      let new_to_add = did_add.iter().flat_map(|path| self.files[path].imports());
      let new_to_add = new_to_add.filter_map(|(_, path)| {
        if added.contains(&path) || self.files.contains_key(&path) {
          None
        } else {
          Some(path)
        }
      });
      let new_to_add: BTreeSet<_> = new_to_add.collect();

      // get the file artifacts for each added file in parallel.
      let iter = new_to_add.into_par_iter().filter_map(|path_id| {
        let path = self.paths().get_path(path_id);
        let art = get_isolated_fs(path, fs)?;
        Some((path_id, art))
      });
      to_add = iter.collect();
    }

    // compute a mapping from path id p to non-empty set of path ids S, s.t. for all q in S, q was
    // just added, and p depends on q.
    let added_dependencies = self.files.par_iter().filter_map(|(&path_id, file)| {
      let dependencies: BTreeSet<_> =
        file.imports().filter_map(|(_, path)| added.contains(&path).then_some(path)).collect();
      if dependencies.is_empty() {
        None
      } else {
        Some((path_id, dependencies))
      }
    });
    let added_dependencies: PathMap<_> = added_dependencies.collect();

    // using that mapping, update the dependents.
    for (&dependent, dependencies) in &added_dependencies {
      for &dependency in dependencies {
        self.dependents.entry(dependency).or_default().insert(dependent);
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
          .chain(art.errors.lex.iter().map(|err| (err.range(), err.to_string())))
          .chain(art.errors.parse.iter().map(|err| (err.range(), err.to_string())))
          .chain(art.errors.desugar.iter().map(|err| (err.range(), err.to_string())))
          .chain(art.errors.statics.iter().map(|err| {
            let expr = err.expr();
            let ptr = art.pointers.get_ptr(expr);
            let err = err.display(&self.artifacts.strings);
            (ptr.text_range(), err.to_string())
          }))
          .filter_map(|(range, message)| {
            let Some(range) = art.pos_db.range_utf16(range) else {
              always!(false, "bad range: {range:?}");
              return None;
            };
            Some(Diagnostic { range, message })
          })
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
  pub fn path_id(&mut self, path: paths::AbsPathBuf) -> PathId {
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
  fn new(
    contents: &str,
    current_dir: &paths::AbsPath,
    fs: &dyn jsonnet_desugar::FileSystem,
  ) -> Self {
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
  fn is_file(&self, p: &std::path::Path) -> bool {
    paths::FileSystem::is_file(self.0, p)
  }
}

fn get_isolated_fs<F>(path: &paths::AbsPath, fs: &F) -> Option<IsolatedFileArtifacts>
where
  F: paths::FileSystem,
{
  let contents = match fs.read_to_string(path.as_path()) {
    Ok(x) => x,
    Err(e) => {
      always!(false, "read to string error: {e}");
      return None;
    }
  };
  get_isolated_str(path, contents.as_str(), fs)
}

fn get_isolated_str<F>(
  path: &paths::AbsPath,
  contents: &str,
  fs: &F,
) -> Option<IsolatedFileArtifacts>
where
  F: paths::FileSystem,
{
  let Some(parent) = path.parent() else {
    let path = path.as_path().display();
    always!(false, "no parent: {path}");
    return None;
  };
  Some(IsolatedFileArtifacts::new(contents, parent, &FsAdapter(fs)))
}
