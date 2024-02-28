//! Analyze jsonnet files.

#![allow(clippy::too_many_lines)]

use always::always;
use diagnostic::Diagnostic;
use jsonnet_syntax::ast::AstNode as _;
use paths::{PathId, PathMap};
use rayon::iter::{IntoParallelIterator as _, IntoParallelRefIterator as _, ParallelIterator as _};
use rustc_hash::{FxHashMap, FxHashSet};
use std::{io, path::Path};

const MAX_DIAGNOSTICS_PER_FILE: usize = 5;

/// Options for initialization.
#[derive(Debug, Default)]
pub struct Init {
  /// Manifest into JSON.
  pub manifest: bool,
  /// Extra directories in which to search for import paths.
  pub root_dirs: Vec<std::path::PathBuf>,
}

/// A trait for a file systems.
pub trait FileSystem: paths::FileSystem {
  /// Read the contents of a file as bytes.
  ///
  /// # Errors
  ///
  /// If the filesystem failed us.
  fn read_to_bytes(&self, path: &Path) -> io::Result<Vec<u8>>;
}

/// The state of analysis.
#[derive(Debug)]
pub struct St {
  manifest: bool,
  root_dirs: Vec<paths::CanonicalPathBuf>,
  artifacts: jsonnet_expr::Artifacts,
  /// these are the non-jsonnet imported files via `importstr`
  importstr: PathMap<String>,
  /// these are the non-jsonnet imported files via `importbin`
  importbin: PathMap<Vec<u8>>,
  /// note: a file may be in files and importstr if it is imported twice in both ways (rare)
  files: PathMap<jsonnet_eval::JsonnetFile>,
  /// invariant: `x` in `files` iff `x` in `files_extra`.
  files_extra: PathMap<FileArtifacts>,
  /// invariant: if `x` in `json`, then `x` in `files`.
  json: PathMap<jsonnet_eval::error::Result<jsonnet_eval::Json>>,
  /// invariants:
  /// - if `x` in `dependents`, at least one of the following is true:
  ///   - `x` in `files`
  ///   - `x` in `importstr`
  ///   - `x` in `importbin`
  /// - if `a` depends on `b`, `a` in `dependents[b]`. (note: NOT a bi-implication)
  ///
  /// NON-invariants:
  /// - for all `(_, s)` in `dependents`, for all `x` in `s`, `x` in `files`.
  dependents: PathMap<FxHashSet<PathId>>,
}

impl St {
  /// Returns a new `St` with the given init options.
  #[must_use]
  pub fn new<F>(fs: &F, init: Init) -> Self
  where
    F: paths::FileSystem,
  {
    log::info!("make new St with {init:?}");
    let mut root_dirs = Vec::<paths::CanonicalPathBuf>::with_capacity(init.root_dirs.len());
    for dir in init.root_dirs {
      match fs.canonical(dir.as_path()) {
        Ok(can) => root_dirs.push(can),
        Err(e) => {
          let dir = dir.display();
          always!(false, "{dir}: i/o error: {e}");
        }
      }
    }
    Self {
      manifest: init.manifest,
      root_dirs,
      artifacts: jsonnet_expr::Artifacts::default(),
      importstr: paths::PathMap::default(),
      importbin: paths::PathMap::default(),
      files: paths::PathMap::default(),
      files_extra: paths::PathMap::default(),
      json: paths::PathMap::default(),
      dependents: paths::PathMap::default(),
    }
  }

  /// Updates the state with added and removed Jsonnet paths.
  #[must_use]
  pub fn update_many<F>(
    &mut self,
    fs: &F,
    remove: Vec<paths::CanonicalPathBuf>,
    add: Vec<paths::CanonicalPathBuf>,
  ) -> PathMap<Vec<Diagnostic>>
  where
    F: Sync + Send + paths::FileSystem,
  {
    log::info!("remove {} files", remove.len());
    // NOTE: for each r in remove, we DO NOT bother removing r from s for any (_, s) in dependents.
    let updated = remove.into_iter().filter_map(|path| {
      let path_id = self.path_id(path);
      let was_in_files = self.files.remove(&path_id).is_some();
      let was_in_files_extra = self.files_extra.remove(&path_id).is_some();
      always!(
        was_in_files == was_in_files_extra,
        "mismatched in-ness for files ({was_in_files}) and extra ({was_in_files_extra})"
      );
      let ret = self.dependents.remove(&path_id);
      always!(
        ret.is_none() || was_in_files,
        "{} was in dependents, but not in files",
        self.artifacts.paths.get_path(path_id).as_path().display()
      );
      ret
    });
    let updated: FxHashSet<_> = updated.flatten().collect();

    log::info!("get {} initial file artifacts in parallel", add.len());
    let file_artifacts = add.into_par_iter().filter_map(|path| {
      let path = path.as_canonical_path();
      let mut art = match get_isolated_fs(path, &self.root_dirs, fs) {
        Ok(x) => x,
        Err(e) => {
          always!(false, "{}: i/o error: {}", path.as_path().display(), e);
          return None;
        }
      };
      let path = art.combine.paths.get_id(path);
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
    path: &paths::CanonicalPath,
    contents: &str,
  ) -> PathMap<Vec<Diagnostic>>
  where
    F: Sync + Send + paths::FileSystem,
  {
    log::info!("update one file: {}", path.as_path().display());
    let mut art = match get_isolated_str(path, contents, &self.root_dirs, fs) {
      Ok(x) => x,
      Err(e) => {
        always!(false, "{}: i/o error: {}", path.as_path().display(), e);
        return PathMap::default();
      }
    };
    // since we have &mut self, and no parallelism (as opposed to in update_many), we could get the
    // path id from self. but instead, we intentionally get the path id from `art`, to uphold the
    // contract of `update`.
    let path_id = art.combine.paths.get_id(path);
    self.update(fs, FxHashSet::default(), vec![(path_id, art)])
  }

  /// invariant: for all `(p, a)` in `to_add`, `p` is the path id, of the path q, **from the path
  /// store contained in a**, of that path q that yielded a. this path id may **or may not**
  /// (usually not) be the path id from the store in self. (a will be combined with the path store
  /// in self, and p will be updated appropriately.)
  fn update<F>(
    &mut self,
    fs: &F,
    mut needs_update: FxHashSet<paths::PathId>,
    mut to_add: Vec<(paths::PathId, IsolatedFileArtifacts)>,
  ) -> PathMap<Vec<Diagnostic>>
  where
    F: Sync + Send + paths::FileSystem,
  {
    let mut added = FxHashSet::<PathId>::default();

    log::info!("repeatedly add the new files and any relevant imports");
    while !to_add.is_empty() {
      let did_add = to_add.drain(..).map(|(mut path_id, mut art)| {
        let subst = jsonnet_expr::Subst::get(&mut self.artifacts, art.combine);
        log::debug!("subst: {subst:?}");

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
      let did_add: FxHashSet<_> = did_add.collect();
      log::info!("did add {} files", did_add.len());

      // the next set of things to add is imports from the added files that do not exist and were
      // not themselves already added.
      //
      // `new_to_add` is the jsonnet importer paths and their imports. the string and binary paths
      // are handled separately.
      //
      // we actually do the importing and processing of the jsonnet paths in parallel. TODO this
      // could be in parallel for string and binary with a bit more work.
      let mut new_to_add = Vec::<(PathId, jsonnet_eval::Import)>::default();
      for &importer in &did_add {
        for import in self.files[&importer].imports() {
          if added.contains(&import.path) || self.files.contains_key(&import.path) {
            continue;
          }
          match import.kind {
            jsonnet_expr::ImportKind::Code => {
              new_to_add.push((importer, import));
            }
            jsonnet_expr::ImportKind::String => {
              let path = self.artifacts.paths.get_path(import.path);
              match fs.read_to_string(path.as_path()) {
                Ok(contents) => {
                  self.importstr.insert(import.path, contents);
                }
                Err(e) => {
                  let Some(fe) = self.files_extra.get_mut(&importer) else {
                    let path = self.artifacts.paths.get_path(importer).as_path().display();
                    always!(false, "no files extra when in files: {path}");
                    continue;
                  };
                  fe.errors.imports.push((import.expr, e));
                }
              }
            }
            jsonnet_expr::ImportKind::Binary => {
              let path = self.artifacts.paths.get_path(import.path);
              match fs.read_to_string(path.as_path()) {
                Ok(contents) => {
                  self.importstr.insert(import.path, contents);
                }
                Err(e) => {
                  let Some(fe) = self.files_extra.get_mut(&importer) else {
                    let path = self.artifacts.paths.get_path(importer).as_path().display();
                    always!(false, "no files extra when in files: {path}");
                    continue;
                  };
                  fe.errors.imports.push((import.expr, e));
                }
              }
            }
          }
        }
      }

      log::info!("get file artifacts for {} files in parallel", new_to_add.len());
      let iter = new_to_add.into_par_iter().map(|(importer, import)| {
        let imported_path = self.artifacts.paths.get_path(import.path);
        match get_isolated_fs(imported_path, &self.root_dirs, fs) {
          Ok(mut art) => {
            // intentionally turn it into the one from `art`
            let art_import_path = art.combine.paths.get_id(imported_path);
            Ok((art_import_path, art))
          }
          Err(e) => Err((importer, import.expr, e)),
        }
      });
      let file_artifacts: Vec<_> = iter.collect();
      for result in file_artifacts {
        match result {
          Ok((path_id, art)) => to_add.push((path_id, art)),
          Err((importer, expr, e)) => {
            let Some(fe) = self.files_extra.get_mut(&importer) else {
              let path = self.artifacts.paths.get_path(importer).as_path().display();
              always!(false, "no files extra when in files: {path}");
              continue;
            };
            fe.errors.imports.push((expr, e));
          }
        }
      }
    }

    // compute a mapping from path id p to non-empty set of path ids S, s.t. for all q in S, q was
    // just added, and p depends on q.
    log::info!("compute dependency mapping on {} files", self.files.len());
    let added_dependencies = self.files.par_iter().filter_map(|(&path, file)| {
      let dependencies: FxHashSet<_> = file
        .imports()
        .filter_map(|import| added.contains(&import.path).then_some(import.path))
        .collect();
      if dependencies.is_empty() {
        None
      } else {
        Some((path, dependencies))
      }
    });
    let added_dependencies: PathMap<_> = added_dependencies.collect();

    log::info!("update {} dependents", added_dependencies.len());
    for (&dependent, dependencies) in &added_dependencies {
      for &dependency in dependencies {
        if dependency != dependent {
          self.dependents.entry(dependency).or_default().insert(dependent);
        }
      }
    }

    // added files need an update.
    needs_update.extend(added);

    let mut updated = FxHashSet::<PathId>::default();

    log::info!("repeatedly update files");
    let mut ret = PathMap::<Vec<Diagnostic>>::default();
    while !needs_update.is_empty() {
      let cx = self.cx();

      log::info!("getting diagnostics for {} files", needs_update.len());
      let iter = needs_update.iter().map(|&path| {
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
          .chain(art.errors.imports.iter().map(|(expr, err)| {
            let ptr = art.pointers.get_ptr(*expr);
            (ptr.text_range(), err.to_string())
          }))
          .filter_map(|(range, message)| {
            let Some(range) = art.pos_db.range_utf16(range) else {
              always!(false, "bad range: {range:?}");
              return None;
            };
            Some(Diagnostic { range, message })
          })
          .take(MAX_DIAGNOSTICS_PER_FILE)
          .collect();
        (path, ds)
      });
      let old_len: usize = ret.par_iter().map(|(_, xs)| xs.len()).sum();
      ret.extend(iter);
      let new_len: usize = ret.par_iter().map(|(_, xs)| xs.len()).sum();
      log::info!("added {} diagnostics", new_len - old_len);

      let updated_vals: PathMap<_> = if self.manifest {
        // manifest in parallel for all updated files. TODO this probably doesn't respect ordering
        // requirements among the updated. what if one updated file depends on another updated file?
        let iter = needs_update.par_iter().filter_map(|&path| {
          // only exec if no errors in the file so far.
          self.files_extra[&path].errors.is_empty().then(|| {
            let val = jsonnet_eval::get_exec(cx, path);
            let val = val.and_then(|val| jsonnet_eval::get_manifest(cx, val));
            (path, val)
          })
        });
        iter.collect()
      } else {
        PathMap::default()
      };
      log::info!("updated {} vals", updated_vals.len());
      self.json.extend(updated_vals);
      updated.extend(needs_update.iter().copied());

      let iter = needs_update
        .iter()
        .flat_map(|&path_id| {
          // TODO could check if new json == old json and not add dependents if same
          let dependents = self.dependents.get(&path_id);
          dependents.into_iter().flatten().copied()
        })
        .filter(|path_id| !updated.contains(path_id));
      needs_update = iter.collect();
      log::info!("found {} dependents", needs_update.len());
    }

    log::info!("finish update for {} files", ret.len());
    ret
  }

  /// Return an evaluation context from this.
  fn cx(&self) -> jsonnet_eval::Cx<'_> {
    jsonnet_eval::Cx {
      jsonnet_files: &self.files,
      importstr: &self.importstr,
      importbin: &self.importbin,
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

  /// Returns the def site for the indicated area, if there is one.
  #[must_use]
  pub fn get_def(
    &self,
    mut path_id: PathId,
    pos: text_pos::PositionUtf16,
  ) -> Option<(paths::PathId, text_pos::RangeUtf16)> {
    let file = self.files_extra.get(&path_id)?;
    let ts = file.pos_db.text_size_utf16(pos)?;
    let root = file.syntax.clone().into_ast()?;
    let tok = jsonnet_syntax::node_token(root.syntax(), ts)?;
    let node = tok.parent()?;
    let ptr = jsonnet_syntax::ast::SyntaxNodePtr::new(&node);
    let expr = file.pointers.get_idx(ptr)?;
    let def = file.defs.get(&expr)?;
    let tr = match *def {
      jsonnet_statics::Def::Builtin => return None,
      jsonnet_statics::Def::ObjectComp(expr) => {
        let obj = file.pointers.get_ptr(expr);
        let obj = obj.cast::<jsonnet_syntax::ast::Object>()?;
        let obj = obj.try_to_node(root.syntax())?;
        let comp_spec = obj.comp_specs().next()?;
        match comp_spec {
          jsonnet_syntax::ast::CompSpec::ForSpec(for_spec) => for_spec.id()?.text_range(),
          jsonnet_syntax::ast::CompSpec::IfSpec(_) => return None,
        }
      }
      jsonnet_statics::Def::Local(expr, idx) => {
        let local = file.pointers.get_ptr(expr);
        // NOTE because of desugaring, not all expr locals are actually from ast locals. we try to
        // get the exact location first and then fall back.
        local
          .cast::<jsonnet_syntax::ast::ExprLocal>()
          .and_then(|local| {
            let local = local.try_to_node(root.syntax())?;
            Some(local.binds().nth(idx)?.id()?.text_range())
          })
          .or_else(|| {
            log::warn!("local fallback: {local:?}");
            let node = local.try_to_node(root.syntax())?;
            log::warn!("node: {node:?}");
            Some(node.text_range())
          })?
      }
      jsonnet_statics::Def::Function(expr, idx) => {
        let func = file.pointers.get_ptr(expr);
        // NOTE because of desugaring, possibly not all expr fns are actually from ast fns
        func
          .cast::<jsonnet_syntax::ast::ExprFunction>()
          .and_then(|func| {
            let func = func.try_to_node(root.syntax())?;
            Some(func.paren_params()?.params().nth(idx)?.id()?.text_range())
          })
          .or_else(|| {
            log::warn!("func fallback: {func:?}");
            let node = func.try_to_node(root.syntax())?;
            log::warn!("node: {node:?}");
            Some(node.text_range())
          })?
      }
      jsonnet_statics::Def::Import(imported) => {
        path_id = imported;
        text_size::TextRange::default()
      }
    };
    let range = file.pos_db.range_utf16(tr)?;
    Some((path_id, range))
  }
}

/// Artifacts from a file whose shared artifacts have been combined into the global ones.
#[derive(Debug)]
#[allow(dead_code)]
struct FileArtifacts {
  pos_db: text_pos::PositionDb,
  syntax: jsonnet_syntax::Root,
  pointers: jsonnet_desugar::Pointers,
  defs: FxHashMap<jsonnet_expr::ExprMust, jsonnet_statics::Def>,
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
  imports: Vec<(jsonnet_expr::ExprMust, io::Error)>,
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
    current_dir: &paths::CanonicalPath,
    other_dirs: &[paths::CanonicalPathBuf],
    fs: &dyn jsonnet_desugar::FileSystem,
  ) -> Self {
    let lex = jsonnet_lex::get(contents);
    let parse = jsonnet_parse::get(&lex.tokens);
    let root = parse.root.clone().into_ast().and_then(|x| x.expr());
    let desugar = jsonnet_desugar::get(current_dir, other_dirs, fs, root);
    let mut st = jsonnet_statics::St::default();
    jsonnet_statics::get(&mut st, &desugar.arenas, desugar.top);
    Self {
      eval: jsonnet_eval::JsonnetFile { expr_ar: desugar.arenas.expr, top: desugar.top },
      combine: jsonnet_expr::Artifacts { paths: desugar.ps, strings: desugar.arenas.str },
      extra: FileArtifacts {
        pos_db: text_pos::PositionDb::new(contents),
        syntax: parse.root,
        pointers: desugar.pointers,
        defs: st.defs,
        errors: FileErrors {
          lex: lex.errors,
          parse: parse.errors,
          desugar: desugar.errors,
          statics: st.errors,
          imports: Vec::new(),
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
  fn canonical(&self, p: &Path) -> io::Result<paths::CanonicalPathBuf> {
    paths::FileSystem::canonical(self.0, p)
  }
}

fn get_isolated_fs<F>(
  path: &paths::CanonicalPath,
  root_dirs: &[paths::CanonicalPathBuf],
  fs: &F,
) -> io::Result<IsolatedFileArtifacts>
where
  F: paths::FileSystem,
{
  let contents = fs.read_to_string(path.as_path())?;
  get_isolated_str(path, contents.as_str(), root_dirs, fs)
}

fn get_isolated_str<F>(
  path: &paths::CanonicalPath,
  contents: &str,
  root_dirs: &[paths::CanonicalPathBuf],
  fs: &F,
) -> io::Result<IsolatedFileArtifacts>
where
  F: paths::FileSystem,
{
  let Some(parent) = path.parent() else {
    return Err(io::Error::other("path has no parent"));
  };
  Ok(IsolatedFileArtifacts::new(contents, parent, root_dirs, &FsAdapter(fs)))
}
