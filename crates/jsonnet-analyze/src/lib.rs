//! Analyze jsonnet files.

#![allow(clippy::too_many_lines)]

mod const_eval;

use always::always;
use diagnostic::Diagnostic;
use jsonnet_syntax::ast::AstNode as _;
use paths::{PathId, PathMap};
use std::collections::hash_map::Entry;
use std::fmt;

/// Options for initialization.
#[derive(Debug, Default)]
pub struct Init {
  /// Path to which other paths may be displayed relative.
  pub relative_to: Option<paths::CleanPathBuf>,
  /// Extra directories in which to search for import paths.
  pub root_dirs: Vec<paths::CleanPathBuf>,
}

/// The state of analysis.
#[derive(Debug)]
pub struct St {
  relative_to: Option<paths::CleanPathBuf>,
  root_dirs: Vec<paths::CleanPathBuf>,
  artifacts: jsonnet_expr::Artifacts,
  open_files: PathMap<String>,
  file_artifacts: PathMap<FileArtifacts>,
  file_exprs: PathMap<jsonnet_eval::JsonnetFile>,
  import_str: PathMap<String>,
  import_bin: PathMap<Vec<u8>>,
}

impl St {
  /// Returns a new `St` with the given init options.
  #[must_use]
  pub fn init(init: Init) -> Self {
    log::info!("make new St with {init:?}");
    Self {
      relative_to: init.relative_to,
      root_dirs: init.root_dirs,
      open_files: PathMap::default(),
      artifacts: jsonnet_expr::Artifacts::default(),
      file_artifacts: PathMap::default(),
      file_exprs: PathMap::default(),
      import_str: PathMap::default(),
      import_bin: PathMap::default(),
    }
  }

  /// Returns the json for this path.
  ///
  /// # Errors
  ///
  /// If this path couldn't be evaluated to json.
  pub fn get_json(&self, path_id: PathId) -> jsonnet_eval::error::Result<&jsonnet_eval::Json> {
    /*
    TODO re-impl
    match self.json.get(&path_id) {
      Some(x) => match x {
        Ok(x) => Ok(x),
        Err(e) => Err(e.clone()),
      },
      None => Err(jsonnet_eval::error::Error::NoPath(path_id)),
    }
     */
    Err(jsonnet_eval::error::Error::NoPath(path_id))
  }

  /// Returns the strings for this.
  #[must_use]
  pub fn strings(&self) -> &jsonnet_expr::StrArena {
    &self.artifacts.strings
  }

  /*
  TODO re-impl/remove
  /// invariant: for all `(p, a)` in `to_add`, `p` is the path id, of the path q, **from the path
  /// store contained in a**, of that path q that yielded a. this path id may **or may not**
  /// (usually not) be the path id from the store in self. (a will be combined with the path store
  /// in self, and p will be updated appropriately.)
  fn update<F>(
    &mut self,
    fs: &F,
    mut needs_update: PathMap<FileErrors>,
    mut to_add: Vec<(PathId, IsolatedFile)>,
    want_diagnostics: bool,
  ) -> PathMap<Vec<Diagnostic>>
  where
    F: Sync + Send + paths::FileSystem,
  {
    let mut added = PathMap::<FileErrors>::default();
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
                    let path = self.display_path_id(importer);
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
                    let path = self.display_path_id(importer);
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

      log::info!("found {} new files to get artifacts in parallel", new_to_add.len());
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
              let path = self.display_path_id(importer);
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
    let cx = jsonnet_eval::Cx {
      jsonnet_files: &self.files,
      importstr: &self.importstr,
      importbin: &self.importbin,
      paths: &self.artifacts.paths,
      str_ar: &self.artifacts.strings,
    };
    while !needs_update.is_empty() {
      let updated_vals: PathMap<_> = if self.manifest {
        // manifest in parallel for all updated files. for those that have errors, we note that fact
        // for later.
        //
        // TODO this probably doesn't respect ordering requirements among the updated. what if one
        // updated file depends on another updated file?
        let iter = needs_update.iter().map(|(&path, errors)| {
          let update = errors.is_empty().then(|| {
            let val = jsonnet_eval::get_exec(cx, path);
            val.and_then(|val| jsonnet_eval::get_manifest(cx, val))
          });
          (path, update)
        });
        iter.collect()
      } else {
        PathMap::default()
      };
      log::info!("updated {} vals", updated_vals.len());
      for (path_id, update) in updated_vals {
        updated.insert(path_id);
        match update {
          Some(result) => {
            self.json.insert(path_id, result);
          }
          None => {
            self.json.remove(&path_id);
          }
        }
      }

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
            .chain(self.json.get(&path).and_then(|res| res.as_ref().err()).and_then(|err| {
              let relative_to = self.relative_to.as_ref().map(paths::CleanPathBuf::as_clean_path);
              let message = err
                .display(&self.artifacts.strings, &self.artifacts.paths, relative_to)
                .to_string();
              let range = match err {
                jsonnet_eval::error::Error::Exec { expr, .. } => {
                  let ptr = art.pointers.get_ptr(*expr);
                  ptr.text_range()
                }
                // should have already been covered by other errors?
                jsonnet_eval::error::Error::NoPath(_) => return None,
                _ => {
                  let syntax = art.syntax.clone().syntax();
                  // try to avoid massive text range
                  syntax.first_child_or_token().map_or(syntax.text_range(), |x| x.text_range())
                }
              };
              Some((range, message))
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
   */

  fn strip<'a>(&self, p: &'a std::path::Path) -> &'a std::path::Path {
    match &self.relative_to {
      None => p,
      Some(r) => p.strip_prefix(r.as_path()).unwrap_or(p),
    }
  }

  fn display_path_id(&self, path_id: PathId) -> impl std::fmt::Display + '_ {
    self.strip(self.artifacts.paths.get_path(path_id).as_path()).display()
  }

  /// NOTE: this brings all the transitive deps of the `path_id` into memory. it is NOT recommended
  /// to bring a massive amount of files into memory at once.
  fn get_all_deps<F>(
    &mut self,
    fs: &F,
    path_id: PathId,
    kind: jsonnet_expr::ImportKind,
  ) -> Result<(), PathIoError>
  where
    F: paths::FileSystem,
  {
    let mut work = vec![(path_id, kind)];
    while let Some((path_id, import_kind)) = work.pop() {
      match import_kind {
        jsonnet_expr::ImportKind::Code => {
          // invariant: file artifacts and file_exprs will BOTH be populated after this
          let file = match (self.file_exprs.entry(path_id), self.file_artifacts.entry(path_id)) {
            (Entry::Occupied(entry), Entry::Occupied(_)) => &*entry.into_mut(),
            (file_entry, arts) => {
              let path = self.artifacts.paths.get_path(path_id).to_owned();
              let path = path.as_clean_path();
              let file = IsolatedFile::from_fs(path, &self.root_dirs, &mut self.artifacts, fs);
              let file = match file {
                Ok(x) => x,
                Err(error) => {
                  return Err(PathIoError { path: self.strip(path.as_path()).to_owned(), error })
                }
              };
              entry_insert(arts, file.artifacts);
              &*entry_insert(file_entry, file.eval)
            }
          };
          work.extend(file.imports().map(|import| (import.path, import_kind)));
        }
        jsonnet_expr::ImportKind::String => match self.import_str.entry(path_id) {
          Entry::Occupied(_) => {}
          Entry::Vacant(entry) => {
            let path = self.artifacts.paths.get_path(path_id).to_owned();
            match fs.read_to_string(path.as_path()) {
              Ok(x) => {
                entry.insert(x);
              }
              Err(error) => {
                return Err(PathIoError { path: self.strip(path.as_path()).to_owned(), error })
              }
            }
          }
        },
        jsonnet_expr::ImportKind::Binary => match self.import_bin.entry(path_id) {
          Entry::Occupied(_) => {}
          Entry::Vacant(entry) => {
            let path = self.artifacts.paths.get_path(path_id).to_owned();
            match fs.read_to_bytes(path.as_path()) {
              Ok(x) => {
                entry.insert(x);
              }
              Err(error) => {
                return Err(PathIoError { path: self.strip(path.as_path()).to_owned(), error })
              }
            }
          }
        },
      }
    }
    Ok(())
  }

  pub(crate) fn get_file_expr(&self, path_id: PathId) -> Option<&jsonnet_eval::JsonnetFile> {
    let ret = self.file_exprs.get(&path_id);
    if ret.is_none() {
      let path = self.display_path_id(path_id);
      always!(false, "no file expr for {path}");
    }
    ret
  }

  pub(crate) fn get_file_artifacts(&self, path_id: PathId) -> Option<&FileArtifacts> {
    let ret = self.file_artifacts.get(&path_id);
    if ret.is_none() {
      let path = self.display_path_id(path_id);
      always!(false, "no file artifacts for {path}");
    }
    ret
  }
}

impl lang_srv_state::State for St {
  fn new<F>(fs: &F, val: Option<serde_json::Value>) -> Self
  where
    F: paths::FileSystem,
  {
    let mut init = Init::default();
    let pwd = fs.current_dir();
    let mut logger_env = env_logger::Env::default();
    // TODO is it correct to use pwd here or should we be using the root dir from the language
    // server init object (which is NOT the `val` init object passed to us here)?
    if let Ok(pwd) = &pwd {
      init.relative_to = Some(pwd.to_owned());
    }
    // TODO report errors from bad init object
    if let Some(serde_json::Value::Object(obj)) = val {
      if let Some(filter) = obj.get("logger_filter").and_then(serde_json::Value::as_str) {
        if !filter.is_empty() {
          logger_env = logger_env.default_filter_or(filter.to_owned());
        }
      }

      init.root_dirs = obj
        .get("root_dirs")
        .and_then(|x| {
          let ary = x.as_array()?;
          let pwd = pwd.ok()?;
          ary
            .iter()
            .map(|x| x.as_str().map(|x| pwd.as_clean_path().join(x)))
            .collect::<Option<Vec<_>>>()
        })
        .unwrap_or_default();
    }

    if let Err(e) = env_logger::try_init_from_env(logger_env) {
      always!(false, "couldn't init logger: {e}");
    }

    Self::init(init)
  }

  const BUG_REPORT_MSG: &'static str = always::BUG_REPORT_MSG;

  const GLOB: &'static str = "**/*.{jsonnet,libsonnet,TEMPLATE}";

  fn is_ext(&self, s: &str) -> bool {
    matches!(s, "jsonnet" | "libsonnet" | "TEMPLATE")
  }

  fn update_many<F>(
    &mut self,
    _fs: &F,
    remove: Vec<paths::CleanPathBuf>,
    add: Vec<paths::CleanPathBuf>,
  ) -> PathMap<Vec<diagnostic::Diagnostic>>
  where
    F: Sync + Send + paths::FileSystem,
  {
    for x in remove.into_iter().chain(add) {
      let path_id = self.path_id(x);
      self.file_artifacts.remove(&path_id);
      self.file_exprs.remove(&path_id);
      self.import_str.remove(&path_id);
      self.import_bin.remove(&path_id);
    }
    PathMap::default()
    /*
    TODO re-impl
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

      self.importstr.remove(&path_id);
      self.importbin.remove(&path_id);

      let was_in_json = self.json.remove(&path_id).is_some();
      always!(
        !was_in_json || was_in_files,
        "{} was in json, but not in files",
        self.display_path_id(path_id)
      );

      let ret = self.dependents.remove(&path_id);
      always!(
        ret.is_none() || was_in_files,
        "{} was in dependents, but not in files",
        self.display_path_id(path_id),
      );
      ret
    });
    // when we remove files, we must reset their diagnostics to nothing.
    let updated: PathMap<_> = updated.flatten().map(|x| (x, FileErrors::default())).collect();

    log::info!("get {} initial file artifacts in parallel", add.len());
    let file_artifacts = add.into_par_iter().filter_map(|path| {
      let path = path.as_clean_path();
      let mut art = match get_isolated_fs(path, &self.root_dirs, fs) {
        Ok(x) => x,
        Err(e) => {
          always!(false, "{}: i/o error: {}", self.strip(path.as_path()).display(), e);
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
     */
  }

  fn update_one<F>(
    &mut self,
    fs: &F,
    path: paths::CleanPathBuf,
    changes: Vec<apply_changes::Change>,
  ) -> PathMap<Vec<diagnostic::Diagnostic>>
  where
    F: Sync + Send + paths::FileSystem,
  {
    log::info!("update one file: {}", self.strip(path.as_path()).display());
    let path_id = self.path_id(path.clone());
    let Some(contents) = self.open_files.get_mut(&path_id) else { return PathMap::default() };
    apply_changes::get(contents, changes);
    let file = IsolatedFile::from_str(
      path.as_clean_path(),
      contents,
      &self.root_dirs,
      &mut self.artifacts,
      fs,
    );
    let file = match file {
      Ok(x) => x,
      Err(e) => {
        // TODO expose a PathIoError?
        always!(false, "{}: i/o error: {}", self.strip(path.as_path()).display(), e);
        return PathMap::default();
      }
    };
    let ds: Vec<_> =
      file_diagnostics(&file.errors, &file.artifacts, &self.artifacts.strings).collect();
    self.file_artifacts.insert(path_id, file.artifacts);
    self.file_exprs.insert(path_id, file.eval);
    PathMap::from_iter([(path_id, ds)])
  }

  /// Opens a path.
  #[must_use]
  fn open<F>(
    &mut self,
    fs: &F,
    path: paths::CleanPathBuf,
    contents: String,
  ) -> PathMap<Vec<diagnostic::Diagnostic>>
  where
    F: Sync + Send + paths::FileSystem,
  {
    let path_id = self.path_id(path.clone());
    self.open_files.insert(path_id, contents);
    self.update_one(fs, path, Vec::new())
  }

  /// Closes a path.
  #[must_use]
  fn close(&mut self, path: paths::CleanPathBuf) -> PathMap<Vec<diagnostic::Diagnostic>> {
    let path_id = self.path_id(path.clone());
    self.open_files.remove(&path_id);
    std::iter::once((path_id, Vec::new())).collect()
  }

  /// Returns whether the path is open.
  #[must_use]
  fn is_open(&mut self, path: &paths::CleanPath) -> bool {
    let path_id = self.path_id(path.to_owned());
    self.open_files.contains_key(&path_id)
  }

  fn hover<F>(
    &mut self,
    _fs: &F,
    _path: paths::CleanPathBuf,
    _pos: text_pos::PositionUtf16,
  ) -> Option<String>
  where
    F: Sync + Send + paths::FileSystem,
  {
    None
    /*
    TODO re-impl
    let path_id = self.path_id(path);
    let json = match self.get_json(path_id) {
      Ok(x) => x,
      Err(e) => {
        // NOTE we don't have the relative_to here
        let e = e.display(&self.artifacts.strings, self.paths(), None);
        log::error!("couldn't get json: {e}",);
        return None;
      }
    };
    Some(json.display(&self.artifacts.strings).to_string())
     */
  }

  /// Get the definition site of a part of a file.
  fn get_def<F>(
    &mut self,
    fs: &F,
    path: paths::CleanPathBuf,
    pos: text_pos::PositionUtf16,
  ) -> Option<(PathId, text_pos::RangeUtf16)>
  where
    F: Sync + Send + paths::FileSystem,
  {
    let path_id = self.path_id(path);
    let ce = {
      let arts = self.get_file_artifacts(path_id)?;
      let ts = arts.pos_db.text_size_utf16(pos)?;
      let root = arts.syntax.clone().into_ast()?;
      let tok = jsonnet_syntax::node_token(root.syntax(), ts)?;
      let node = tok.parent()?;
      let ptr = jsonnet_syntax::ast::SyntaxNodePtr::new(&node);
      let expr = arts.pointers.get_idx(ptr);
      // TODO expose any errors here?
      self.get_all_deps(fs, path_id, jsonnet_expr::ImportKind::Code).ok()?;
      const_eval::get(&*self, path_id, expr)?
    };
    let arts = self.get_file_artifacts(ce.path_id)?;
    let root = arts.syntax.clone().into_ast()?;
    let tr = match ce.kind {
      const_eval::Kind::Expr => arts.pointers.get_ptr(ce.expr).text_range(),
      const_eval::Kind::ObjectCompId => {
        let obj = arts.pointers.get_ptr(ce.expr);
        let obj = obj.cast::<jsonnet_syntax::ast::Object>()?;
        let obj = obj.try_to_node(root.syntax())?;
        let comp_spec = obj.comp_specs().next()?;
        match comp_spec {
          jsonnet_syntax::ast::CompSpec::ForSpec(for_spec) => for_spec.id()?.text_range(),
          jsonnet_syntax::ast::CompSpec::IfSpec(_) => return None,
        }
      }
      const_eval::Kind::LocalBind(idx) => {
        let local = arts.pointers.get_ptr(ce.expr);
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
        let func = arts.pointers.get_ptr(ce.expr);
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
    let range = arts.pos_db.range_utf16(tr)?;
    Some((ce.path_id, range))
  }

  fn paths(&self) -> &paths::Store {
    &self.artifacts.paths
  }

  fn path_id(&mut self, path: paths::CleanPathBuf) -> PathId {
    self.artifacts.paths.get_id_owned(path)
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

/// A single isolated file.
struct IsolatedFile {
  artifacts: FileArtifacts,
  errors: FileErrors,
  eval: jsonnet_eval::JsonnetFile,
}

impl IsolatedFile {
  fn new(
    contents: &str,
    current_dir: &paths::CleanPath,
    other_dirs: &[paths::CleanPathBuf],
    artifacts: &mut jsonnet_expr::Artifacts,
    fs: &dyn jsonnet_desugar::FileSystem,
  ) -> Self {
    let lex = jsonnet_lex::get(contents);
    let parse = jsonnet_parse::get(&lex.tokens);
    let root = parse.root.clone().into_ast().and_then(|x| x.expr());
    let desugar = jsonnet_desugar::get(current_dir, other_dirs, fs, root);
    let mut st = jsonnet_statics::St::default();
    jsonnet_statics::get(&mut st, &desugar.arenas, desugar.top);
    let combine = jsonnet_expr::Artifacts { paths: desugar.ps, strings: desugar.arenas.str };
    let mut ret = Self {
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
      },
      eval: jsonnet_eval::JsonnetFile { expr_ar: desugar.arenas.expr, top: desugar.top },
    };
    let subst = jsonnet_expr::Subst::get(artifacts, combine);
    for err in &mut ret.errors.statics {
      err.apply(&subst);
    }
    for (_, ed) in ret.eval.expr_ar.iter_mut() {
      ed.apply(&subst);
    }
    for def in ret.artifacts.defs.values_mut() {
      def.apply(&subst);
    }
    ret
  }

  fn from_fs<F>(
    path: &paths::CleanPath,
    root_dirs: &[paths::CleanPathBuf],
    artifacts: &mut jsonnet_expr::Artifacts,
    fs: &F,
  ) -> std::io::Result<IsolatedFile>
  where
    F: paths::FileSystem,
  {
    let contents = fs.read_to_string(path.as_path())?;
    Self::from_str(path, contents.as_str(), root_dirs, artifacts, fs)
  }

  fn from_str<F>(
    path: &paths::CleanPath,
    contents: &str,
    root_dirs: &[paths::CleanPathBuf],
    artifacts: &mut jsonnet_expr::Artifacts,
    fs: &F,
  ) -> std::io::Result<IsolatedFile>
  where
    F: paths::FileSystem,
  {
    let Some(parent) = path.parent() else {
      return Err(std::io::Error::other("path has no parent"));
    };
    Ok(Self::new(contents, parent, root_dirs, artifacts, &FsAdapter(fs)))
  }
}

fn file_diagnostics<'a>(
  errors: &'a FileErrors,
  art: &'a FileArtifacts,
  strings: &'a jsonnet_expr::StrArena,
) -> impl Iterator<Item = Diagnostic> + 'a {
  std::iter::empty()
    .chain(errors.lex.iter().map(|err| (err.range(), err.to_string())))
    .chain(errors.parse.iter().map(|err| (err.range(), err.to_string())))
    .chain(errors.desugar.iter().map(|err| (err.range(), err.to_string())))
    .chain(errors.statics.iter().map(|err| {
      let expr = err.expr();
      let ptr = art.pointers.get_ptr(expr);
      let err = err.display(strings);
      (ptr.text_range(), err.to_string())
    }))
    .filter_map(|(range, message)| {
      let Some(range) = art.pos_db.range_utf16(range) else {
        always!(false, "bad range: {range:?}");
        return None;
      };
      Some(Diagnostic { range, message })
    })
}

/// An I/O error with an associated path.
#[derive(Debug)]
pub struct PathIoError {
  path: std::path::PathBuf,
  error: std::io::Error,
}

impl fmt::Display for PathIoError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}: I/O error: {}", self.path.display(), self.error)
  }
}

impl std::error::Error for PathIoError {
  fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
    Some(&self.error)
  }
}

fn entry_insert<K, V>(entry: Entry<'_, K, V>, value: V) -> &mut V {
  match entry {
    Entry::Occupied(mut entry) => {
      entry.insert(value);
      entry.into_mut()
    }
    Entry::Vacant(entry) => entry.insert(value),
  }
}
