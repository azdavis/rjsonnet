//! Analyze jsonnet files.

#![allow(clippy::too_many_lines)]

mod const_eval;

use always::always;
use diagnostic::Diagnostic;
use jsonnet_syntax::ast::AstNode as _;
use paths::{PathId, PathMap};
use rayon::iter::{IntoParallelIterator as _, IntoParallelRefIterator as _, ParallelIterator as _};
use rustc_hash::FxHashSet;
use std::{fmt, io, path::Path};

/// Options for initialization.
#[derive(Debug, Default)]
pub struct Init {
  /// Manifest into JSON.
  pub manifest: bool,
  /// Extra directories in which to search for import paths.
  pub root_dirs: Vec<paths::CleanPathBuf>,
  /// How to show diagnostics.
  pub show_diagnostics: ShowDiagnostics,
  /// Maximum number of diagnostics per file we may show.
  pub max_diagnostics_per_file: DefaultUsize<5>,
}

/// How to show diagnostics.
#[derive(Debug, Default)]
pub enum ShowDiagnostics {
  /// On all files in the project.
  All,
  /// Only on open files.
  #[default]
  Open,
  /// On no files.
  None,
}

impl std::str::FromStr for ShowDiagnostics {
  type Err = ShowDiagnosticsFromStrError;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    let ret = match s {
      "all" => Self::All,
      "open" => Self::Open,
      "none" => Self::None,
      _ => return Err(ShowDiagnosticsFromStrError(())),
    };
    Ok(ret)
  }
}

/// An error used in `impl FromStr for ShowDiagnostics`.
#[derive(Debug)]
pub struct ShowDiagnosticsFromStrError(());

impl fmt::Display for ShowDiagnosticsFromStrError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.write_str("invalid value for show diagnostics: must be one of 'all', 'open', or 'none'")
  }
}

/// Same as a `usize` but carries the default value in a const generic.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct DefaultUsize<const DEFAULT: usize>(pub usize);

impl<const DEFAULT: usize> Default for DefaultUsize<DEFAULT> {
  fn default() -> Self {
    Self(DEFAULT)
  }
}

impl<const DEFAULT: usize> From<usize> for DefaultUsize<DEFAULT> {
  fn from(value: usize) -> Self {
    Self(value)
  }
}

impl<const DEFAULT: usize> std::str::FromStr for DefaultUsize<DEFAULT> {
  type Err = <usize as std::str::FromStr>::Err;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    s.parse::<usize>().map(Self)
  }
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
  root_dirs: Vec<paths::CleanPathBuf>,
  show_diagnostics: ShowDiagnostics,
  max_diagnostics_per_file: usize,
  artifacts: jsonnet_expr::Artifacts,
  /// these are the non-jsonnet imported files via `importstr`
  importstr: PathMap<String>,
  /// these are the non-jsonnet imported files via `importbin`
  importbin: PathMap<Vec<u8>>,
  /// note: a file may be in files and importstr if it is imported twice in both ways (rare)
  files: PathMap<jsonnet_eval::JsonnetFile>,
  /// invariant: `x` in `files` iff `x` in `file_artifacts`.
  file_artifacts: PathMap<FileArtifacts>,
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
  pub fn new(init: Init) -> Self {
    log::info!("make new St with {init:?}");
    Self {
      manifest: init.manifest,
      root_dirs: init.root_dirs,
      show_diagnostics: init.show_diagnostics,
      max_diagnostics_per_file: init.max_diagnostics_per_file.0,
      artifacts: jsonnet_expr::Artifacts::default(),
      importstr: paths::PathMap::default(),
      importbin: paths::PathMap::default(),
      files: paths::PathMap::default(),
      file_artifacts: paths::PathMap::default(),
      json: paths::PathMap::default(),
      dependents: paths::PathMap::default(),
    }
  }

  /// Updates the state with added and removed Jsonnet paths.
  #[must_use]
  pub fn update_many<F>(
    &mut self,
    fs: &F,
    remove: Vec<paths::CleanPathBuf>,
    add: Vec<paths::CleanPathBuf>,
  ) -> PathMap<Vec<Diagnostic>>
  where
    F: Sync + Send + paths::FileSystem,
  {
    log::info!("remove {} files", remove.len());
    // NOTE: for each r in remove, we DO NOT bother removing r from s for any (_, s) in dependents.
    let updated = remove.into_iter().filter_map(|path| {
      let path_id = self.path_id(path);
      let was_in_files = self.files.remove(&path_id).is_some();
      let was_in_files_artifacts = self.file_artifacts.remove(&path_id).is_some();
      always!(
        was_in_files == was_in_files_artifacts,
        "mismatched in-ness for files ({was_in_files}) and artifacts ({was_in_files_artifacts})"
      );
      let ret = self.dependents.remove(&path_id);
      always!(
        ret.is_none() || was_in_files,
        "{} was in dependents, but not in files",
        self.artifacts.paths.get_path(path_id).as_path().display()
      );
      ret
    });
    // when we remove files, we must reset their diagnostics to nothing.
    let updated: paths::PathMap<_> =
      updated.flatten().map(|x| (x, FileErrors::default())).collect();

    log::info!("get {} initial file artifacts in parallel", add.len());
    let file_artifacts = add.into_par_iter().filter_map(|path| {
      let path = path.as_clean_path();
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
    // NOTE depends on the fact that all the added files in `update_all` are NOT open
    let want_diagnostics = matches!(self.show_diagnostics, ShowDiagnostics::All);
    self.update(fs, updated, file_artifacts, want_diagnostics)
  }

  /// Updates one file.
  #[must_use]
  pub fn update_one<F>(
    &mut self,
    fs: &F,
    path: &paths::CleanPath,
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
    // NOTE depends on the fact that `update_one` is called iff the file is open
    let want_diagnostics =
      matches!(self.show_diagnostics, ShowDiagnostics::All | ShowDiagnostics::Open);
    self.update(fs, paths::PathMap::default(), vec![(path_id, art)], want_diagnostics)
  }

  /// invariant: for all `(p, a)` in `to_add`, `p` is the path id, of the path q, **from the path
  /// store contained in a**, of that path q that yielded a. this path id may **or may not**
  /// (usually not) be the path id from the store in self. (a will be combined with the path store
  /// in self, and p will be updated appropriately.)
  fn update<F>(
    &mut self,
    fs: &F,
    mut needs_update: paths::PathMap<FileErrors>,
    mut to_add: Vec<(paths::PathId, IsolatedFile)>,
    want_diagnostics: bool,
  ) -> PathMap<Vec<Diagnostic>>
  where
    F: Sync + Send + paths::FileSystem,
  {
    let mut added = paths::PathMap::<FileErrors>::default();
    log::info!("repeatedly add the new files and any relevant imports");
    while !to_add.is_empty() {
      let did_add = to_add.drain(..).map(|(mut path_id, mut art)| {
        let subst = jsonnet_expr::Subst::get(&mut self.artifacts, art.combine);
        log::debug!("subst: {subst:?}");

        for (_, ed) in art.eval.expr_ar.iter_mut() {
          ed.apply(&subst);
        }
        for err in &mut art.errors.statics {
          err.apply(&subst);
        }
        for def in art.artifacts.defs.values_mut() {
          def.apply(&subst);
        }
        path_id = subst.get_path_id(path_id);

        self.files.insert(path_id, art.eval);
        self.file_artifacts.insert(path_id, art.artifacts);
        added.insert(path_id, art.errors);

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
          if added.contains_key(&import.path) || self.files.contains_key(&import.path) {
            continue;
          }
          match import.kind {
            jsonnet_expr::ImportKind::Code => new_to_add.push((importer, import)),
            jsonnet_expr::ImportKind::String => {
              let path = self.artifacts.paths.get_path(import.path);
              match fs.read_to_string(path.as_path()) {
                Ok(contents) => {
                  self.importstr.insert(import.path, contents);
                }
                Err(e) => {
                  let Some(errors) = added.get_mut(&importer) else {
                    let path = self.artifacts.paths.get_path(importer).as_path().display();
                    always!(false, "couldn't get errors: {path}");
                    continue;
                  };
                  errors.imports.push((import.expr, e));
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
                  let Some(errors) = added.get_mut(&importer) else {
                    let path = self.artifacts.paths.get_path(importer).as_path().display();
                    always!(false, "couldn't get errors: {path}");
                    continue;
                  };
                  errors.imports.push((import.expr, e));
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
            let Some(errors) = added.get_mut(&importer) else {
              let path = self.artifacts.paths.get_path(importer).as_path().display();
              always!(false, "couldn't get errors: {path}");
              continue;
            };
            errors.imports.push((expr, e));
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
        .filter_map(|import| added.contains_key(&import.path).then_some(import.path))
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
      if want_diagnostics {
        log::info!("getting diagnostics for {} files", needs_update.len());
        let iter = needs_update.iter().map(|(&path, errors)| {
          let art = &self.file_artifacts[&path];
          let ds: Vec<_> = std::iter::empty()
            .chain(errors.lex.iter().map(|err| (err.range(), err.to_string())))
            .chain(errors.parse.iter().map(|err| (err.range(), err.to_string())))
            .chain(errors.desugar.iter().map(|err| (err.range(), err.to_string())))
            .chain(errors.statics.iter().map(|err| {
              let expr = err.expr();
              let ptr = art.pointers.get_ptr(expr);
              let err = err.display(&self.artifacts.strings);
              (ptr.text_range(), err.to_string())
            }))
            .chain(errors.imports.iter().map(|(expr, err)| {
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
            .take(self.max_diagnostics_per_file)
            .collect();
          (path, ds)
        });
        let old_len: usize = ret.par_iter().map(|(_, xs)| xs.len()).sum();
        ret.extend(iter);
        let new_len: usize = ret.par_iter().map(|(_, xs)| xs.len()).sum();
        log::info!("added {} diagnostics", new_len - old_len);
      }

      let updated_vals: PathMap<_> = if self.manifest {
        let cx = self.cx();
        // manifest in parallel for all updated files. TODO this probably doesn't respect ordering
        // requirements among the updated. what if one updated file depends on another updated file?
        let iter = needs_update.par_iter().filter_map(|(&path, errors)| {
          // only exec if no errors in the file so far.
          errors.is_empty().then(|| {
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
      updated.extend(needs_update.keys().copied());

      let iter = needs_update
        .keys()
        .flat_map(|&path_id| {
          // TODO could check if new json == old json and not add dependents if same
          let dependents = self.dependents.get(&path_id);
          dependents.into_iter().flatten().copied()
        })
        .filter(|path_id| !updated.contains(path_id))
        .map(|x| (x, FileErrors::default()));

      // TODO: when we do FileErrors::default above, that is probably not correct. we should
      // probably instead compute the file errors for the new set of need-update files.
      //
      // in fact, we should probably compute some or all of the other isolated file artifacts, like
      // the eval result for that file. since that could change if the imported files for that file
      // changed. and we know that the imported files did change, because the new round of
      // needs_update is computed as the (transitive) dependents of all the changed (updated) files.
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
  pub fn path_id(&mut self, path: paths::CleanPathBuf) -> PathId {
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
    path_id: PathId,
    pos: text_pos::PositionUtf16,
  ) -> Option<(paths::PathId, text_pos::RangeUtf16)> {
    let ce = {
      let art = self.file_artifacts.get(&path_id)?;
      let ts = art.pos_db.text_size_utf16(pos)?;
      let root = art.syntax.clone().into_ast()?;
      let tok = jsonnet_syntax::node_token(root.syntax(), ts)?;
      let node = tok.parent()?;
      let ptr = jsonnet_syntax::ast::SyntaxNodePtr::new(&node);
      let expr = art.pointers.get_idx(ptr);
      const_eval::get(self, path_id, expr)?
    };
    let art = self.file_artifacts.get(&ce.ewp.path_id)?;
    let root = art.syntax.clone().into_ast()?;
    let tr = match ce.kind {
      const_eval::Kind::Expr => art.pointers.get_ptr(ce.ewp.expr).text_range(),
      const_eval::Kind::ObjectCompId => {
        let obj = art.pointers.get_ptr(ce.ewp.expr);
        let obj = obj.cast::<jsonnet_syntax::ast::Object>()?;
        let obj = obj.try_to_node(root.syntax())?;
        let comp_spec = obj.comp_specs().next()?;
        match comp_spec {
          jsonnet_syntax::ast::CompSpec::ForSpec(for_spec) => for_spec.id()?.text_range(),
          jsonnet_syntax::ast::CompSpec::IfSpec(_) => return None,
        }
      }
      const_eval::Kind::LocalBind(idx) => {
        let local = art.pointers.get_ptr(ce.ewp.expr);
        // NOTE because of desugaring, not all expr locals are actually from ast locals. we try to
        // get the exact location first and then fall back.
        local
          .cast::<jsonnet_syntax::ast::ExprLocal>()
          .and_then(|local| {
            let local = local.try_to_node(root.syntax())?;
            Some(local.bind_commas().nth(idx)?.bind()?.id()?.text_range())
          })
          .or_else(|| {
            log::warn!("local fallback: {local:?}");
            let node = local.try_to_node(root.syntax())?;
            log::warn!("node: {node:?}");
            Some(node.text_range())
          })?
      }
      const_eval::Kind::FunctionParam(idx) => {
        let func = art.pointers.get_ptr(ce.ewp.expr);
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
    };
    let range = art.pos_db.range_utf16(tr)?;
    Some((ce.ewp.path_id, range))
  }
}

/// Artifacts from a file whose shared artifacts have been combined into the global ones.
#[derive(Debug)]
struct FileArtifacts {
  pos_db: text_pos::PositionDb,
  syntax: jsonnet_syntax::Root,
  pointers: jsonnet_desugar::Pointers,
  defs: jsonnet_statics::DefMap,
}

/// Errors from a file analyzed in isolation.
#[derive(Debug, Default)]
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
      && self.imports.is_empty()
  }
}

/// Artifacts from a file analyzed in isolation.
#[derive(Debug)]
struct IsolatedFile {
  eval: jsonnet_eval::JsonnetFile,
  combine: jsonnet_expr::Artifacts,
  artifacts: FileArtifacts,
  errors: FileErrors,
}

impl IsolatedFile {
  /// Returns artifacts for a file contained in the given directory.
  fn new(
    contents: &str,
    current_dir: &paths::CleanPath,
    other_dirs: &[paths::CleanPathBuf],
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
      artifacts: FileArtifacts {
        pos_db: text_pos::PositionDb::new(contents),
        syntax: parse.root,
        pointers: desugar.pointers,
        defs: st.defs,
      },
      errors: FileErrors {
        lex: lex.errors,
        parse: parse.errors,
        desugar: desugar.errors,
        statics: st.errors,
        imports: Vec::new(),
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
  fn is_file(&self, p: &Path) -> bool {
    paths::FileSystem::is_file(self.0, p)
  }
}

fn get_isolated_fs<F>(
  path: &paths::CleanPath,
  root_dirs: &[paths::CleanPathBuf],
  fs: &F,
) -> io::Result<IsolatedFile>
where
  F: paths::FileSystem,
{
  let contents = fs.read_to_string(path.as_path())?;
  get_isolated_str(path, contents.as_str(), root_dirs, fs)
}

fn get_isolated_str<F>(
  path: &paths::CleanPath,
  contents: &str,
  root_dirs: &[paths::CleanPathBuf],
  fs: &F,
) -> io::Result<IsolatedFile>
where
  F: paths::FileSystem,
{
  let Some(parent) = path.parent() else {
    return Err(io::Error::other("path has no parent"));
  };
  Ok(IsolatedFile::new(contents, parent, root_dirs, &FsAdapter(fs)))
}
