//! The state of analysis.

use crate::util::{GlobalArtifacts, PathIoError, Result};
use crate::{const_eval, util};
use always::always;
use jsonnet_syntax::ast::AstNode as _;
use jsonnet_ty::display::MultiLine;
use paths::{PathId, PathMap};
use rayon::iter::{IntoParallelIterator as _, ParallelIterator as _};
use rustc_hash::FxHashSet;
use std::collections::hash_map::Entry;

/// The part of the state that vaguely has to do with the filesystem.
///
/// But honestly, the only real reason this exists is so we can factor some things into methods
/// without getting annoying borrowing errors when shoving everything on [`St`].
///
/// Essentially, by having a separate struct and a method that takes `&mut self`, we can ensure the
/// borrow checker knows that only those fields may be affected.
#[derive(Debug)]
struct WithFs {
  relative_to: Option<paths::CleanPathBuf>,
  root_dirs: Vec<paths::CleanPathBuf>,
  artifacts: GlobalArtifacts,
  file_tys: paths::PathMap<jsonnet_ty::Ty>,
  /// INVARIANT: this is exactly the set of files that do have errors that have been loaded into
  /// either `file_artifacts` or `file_exprs` on the [`St`] that contains this.
  has_errors: paths::PathSet,
}

impl WithFs {
  fn strip<'a>(&self, p: &'a std::path::Path) -> &'a std::path::Path {
    match &self.relative_to {
      None => p,
      Some(r) => p.strip_prefix(r.as_path()).unwrap_or(p),
    }
  }

  fn display_path_id(&self, p: PathId) -> impl std::fmt::Display + '_ {
    self.strip(self.artifacts.syntax.paths.get_path(p).as_path()).display()
  }

  fn get_one_file<F>(&mut self, fs: &F, path_id: PathId) -> Result<util::StaticsFile>
  where
    F: Sync + paths::FileSystem,
  {
    self.ensure_import_tys_cached(fs, path_id, None);
    let path = self.artifacts.syntax.paths.get_path(path_id);
    let res = match util::SyntaxFileToCombine::from_fs(path, &self.root_dirs, fs) {
      Ok(x) => x,
      Err(error) => return Err(PathIoError { path: path.to_owned().into_path_buf(), error }),
    };
    let res = res.combine(&mut self.artifacts);
    let res = util::StaticsFileToCombine::new(res, &self.artifacts, &self.file_tys);
    let res = res.combine(&mut self.artifacts);
    if res.is_clean() {
      self.has_errors.remove(&path_id);
    } else {
      self.has_errors.insert(path_id);
    }
    Ok(res)
  }

  fn get_file_expr<'fe, F>(
    &mut self,
    file_exprs: &'fe mut PathMap<jsonnet_eval::Exprs>,
    fs: &F,
    path_id: PathId,
  ) -> Result<&'fe jsonnet_eval::Exprs>
  where
    F: Sync + paths::FileSystem,
  {
    match file_exprs.entry(path_id) {
      Entry::Occupied(entry) => Ok(&*entry.into_mut()),
      Entry::Vacant(entry) => {
        let file = self.get_one_file(fs, path_id)?;
        Ok(&*entry.insert(file.syntax.exprs))
      }
    }
  }

  /// TODO there is no easy way to do this caching without repeatedly getting the same file if you
  /// want BOTH the expr AND the artifacts
  fn get_file_artifacts<'fa, F>(
    &mut self,
    file_artifacts: &'fa mut PathMap<util::FileArtifacts>,
    fs: &F,
    path_id: PathId,
  ) -> Result<&'fa util::FileArtifacts>
  where
    F: Sync + paths::FileSystem,
  {
    match file_artifacts.entry(path_id) {
      Entry::Occupied(entry) => Ok(&*entry.into_mut()),
      Entry::Vacant(entry) => {
        let file = self.get_one_file(fs, path_id)?;
        Ok(&*entry.insert(file.into_artifacts()))
      }
    }
  }

  #[allow(clippy::too_many_lines)]
  fn ensure_import_tys_cached<F>(
    &mut self,
    fs: &F,
    orig_path_id: PathId,
    mut contents: Option<&str>,
  ) where
    F: Sync + paths::FileSystem,
  {
    log::debug!(
      "ensure_import_tys_cached {:?} {}",
      orig_path_id,
      self.display_path_id(orig_path_id)
    );
    self.file_tys.remove(&orig_path_id);
    let mut work = vec![Action::start(orig_path_id)];
    let mut cur = paths::PathSet::default();
    let mut done = paths::PathSet::default();
    // INVARIANT: level_idx = how many Ends are in work.
    let mut level_idx = 0usize;
    let mut levels = Vec::<paths::PathSet>::new();
    while let Some(Action(path_id, kind)) = work.pop() {
      log::debug!("{kind:?} {path_id:?} {}", self.display_path_id(path_id));
      match kind {
        ActionKind::Start => {
          if done.contains(&path_id) {
            log::info!("already done");
            continue;
          }
          if self.file_tys.contains_key(&path_id) {
            log::info!("already cached");
            // TODO: invalidate cache when files change on disk.
            continue;
          }
          if !cur.insert(path_id) {
            log::warn!("cycle");
            continue;
          }
          work.push(Action::end(path_id));
          level_idx += 1;
          let path = self.artifacts.syntax.paths.get_path(path_id);
          let parent = util::path_parent_must(path);
          let fs_contents: String;
          let contents = match contents.take() {
            Some(x) => {
              always!(path_id == orig_path_id);
              x
            }
            None => match fs.read_to_string(path.as_path()) {
              Ok(x) => {
                fs_contents = x;
                fs_contents.as_str()
              }
              Err(e) => {
                log::warn!("io error: {e}");
                continue;
              }
            },
          };
          // need to make this `parent` owned...
          let parent = parent.to_owned();
          // ...and then shadow it with the borrowed form...
          let parent = parent.as_clean_path();
          let imports = util::approximate_code_imports(contents);
          log::debug!("imports: {imports:?}");
          for import in imports {
            let import = std::path::Path::new(import.as_str());
            // ...and re-make this `dirs` every time...
            let dirs = jsonnet_resolve_import::NonEmptyDirs::new(parent, &self.root_dirs);
            let import = jsonnet_resolve_import::get(import, dirs.iter(), &util::FsAdapter(fs));
            let Some(import) = import else { continue };
            log::debug!("new import: {}", import.as_clean_path().as_path().display());
            // ...because we mutate `paths` here.
            let import = self.artifacts.syntax.paths.get_id_owned(import);
            // this mutation also makes it too annoying to write this for-push as a
            // iter-filter-map-extend.
            work.push(Action::start(import));
          }
        }
        ActionKind::End => {
          level_idx = match level_idx.checked_sub(1) {
            None => {
              always!(false, "End should have a matching Start");
              continue;
            }
            Some(x) => x,
          };
          if level_idx >= levels.len() {
            levels.resize_with(level_idx + 1, paths::PathSet::default);
          }
          let Some(level) = levels.get_mut(level_idx) else {
            always!(false, "levels should have been resized to len at least level + 1");
            continue;
          };
          level.insert(path_id);
          always!(cur.remove(&path_id), "should only End when in cur");
          always!(done.insert(path_id), "should not End if already done");
        }
      }
    }
    always!(level_idx == 0);
    always!(cur.is_empty());
    if cfg!(debug_assertions) {
      let all_levels: paths::PathSet = levels.iter().flatten().copied().collect();
      assert_eq!(all_levels, done);
      let all_levels_count: usize = levels.iter().map(paths::PathSet::len).sum();
      assert_eq!(all_levels_count, done.len());
    }
    if let Some(fst) = levels.first_mut() {
      // remove the original path id since we handle that specially, outside of this fn. e.g. we
      // want ALL of its artifacts. this fn is just for caching the types.
      always!(fst.remove(&orig_path_id));
    } else {
      always!(false, "should have a first level");
    }
    drop(work);
    drop(cur);
    drop(done);
    log::debug!("levels: {levels:?}");
    // reverse so we do the leaves first. TODO ask for less analysis to just get the types
    for level in levels.into_iter().rev() {
      // par
      let syntax_files = level.into_par_iter().filter_map(|path_id| {
        let path = self.artifacts.syntax.paths.get_path(path_id);
        let res = util::SyntaxFileToCombine::from_fs(path, &self.root_dirs, fs).ok()?;
        Some((path_id, res))
      });
      // unzip so we don't have to carry around the path_id unchanged in the next few
      // transformations, in which order will be preserved.
      let (order, syntax_files): (Vec<_>, Vec<_>) = syntax_files.unzip();
      // seq
      let syntax_files = syntax_files.into_iter().map(|res| res.combine(&mut self.artifacts));
      let syntax_files: Vec<_> = syntax_files.collect();
      // par
      let statics_files = syntax_files
        .into_par_iter()
        .map(|res| util::StaticsFileToCombine::new(res, &self.artifacts, &self.file_tys));
      let statics_files: Vec<_> = statics_files.collect();
      always!(order.len() == statics_files.len());
      // seq
      let new_file_tys = order.into_iter().zip(statics_files).filter_map(|(path_id, res)| {
        let res = res.combine(&mut self.artifacts);
        let top_expr = res.syntax.exprs.top?;
        let &top_ty = res.statics.expr_tys.get(&top_expr)?;
        Some((path_id, top_ty))
      });
      self.file_tys.extend(new_file_tys);
    }
  }
}

#[derive(Debug)]
enum ActionKind {
  Start,
  End,
}

#[derive(Debug)]
struct Action(PathId, ActionKind);

impl Action {
  const fn start(p: PathId) -> Self {
    Self(p, ActionKind::Start)
  }

  const fn end(p: PathId) -> Self {
    Self(p, ActionKind::End)
  }
}

/// The state of analysis.
#[derive(Debug)]
pub struct St {
  with_fs: WithFs,
  open_files: PathMap<String>,
  file_artifacts: PathMap<util::FileArtifacts>,
  file_exprs: PathMap<jsonnet_eval::Exprs>,
  import_str: PathMap<String>,
  import_bin: PathMap<Vec<u8>>,
  manifest: bool,
  debug: bool,
  multi_line: MultiLine,
}

impl St {
  /// Returns a new `St` with the given init options.
  #[must_use]
  pub fn init(init: util::Init) -> Self {
    log::info!("make new St with {init:?}");
    Self {
      with_fs: WithFs {
        relative_to: init.relative_to,
        root_dirs: init.root_dirs,
        artifacts: GlobalArtifacts::default(),
        file_tys: paths::PathMap::default(),
        has_errors: paths::PathSet::default(),
      },
      open_files: PathMap::default(),
      file_artifacts: PathMap::default(),
      file_exprs: PathMap::default(),
      import_str: PathMap::default(),
      import_bin: PathMap::default(),
      manifest: init.manifest,
      debug: init.debug,
      multi_line: init.multi_line,
    }
  }

  /// Returns the json for this path.
  ///
  /// # Errors
  ///
  /// If this path couldn't be evaluated to json.
  pub fn get_json(&self, path_id: PathId) -> jsonnet_eval::error::Result<jsonnet_eval::Json> {
    if self.with_fs.has_errors.contains(&path_id) {
      return Err(jsonnet_eval::error::Error::HasErrors(path_id));
    }
    let cx = jsonnet_eval::Cx {
      paths: &self.with_fs.artifacts.syntax.paths,
      str_ar: &self.with_fs.artifacts.syntax.strings,
      exprs: &self.file_exprs,
      import_str: &self.import_str,
      import_bin: &self.import_bin,
    };
    let val = jsonnet_eval::get_exec(cx, path_id)?;
    jsonnet_eval::get_manifest(cx, val)
  }

  /// Returns the strings for this.
  #[must_use]
  pub fn strings(&self) -> &jsonnet_expr::StrArena {
    &self.with_fs.artifacts.syntax.strings
  }

  /// Brings all the transitive deps of the Jsonnet file with path id `path_id` into memory.
  ///
  /// It is NOT recommended to bring a massive amount of files into memory at once.
  ///
  /// # Errors
  ///
  /// When a path had an I/O error.
  pub fn get_all_deps<F>(&mut self, fs: &F, path_id: PathId) -> Result<()>
  where
    F: Sync + paths::FileSystem,
  {
    let mut work = vec![(path_id, jsonnet_expr::ImportKind::Code)];
    let mut seen = FxHashSet::<(PathId, jsonnet_expr::ImportKind)>::default();
    while let Some((path_id, import_kind)) = work.pop() {
      // prevent infinite looping on import cycles. no need to report a cycle error here - we'll
      // error when evaluating.
      if !seen.insert((path_id, import_kind)) {
        continue;
      }
      match import_kind {
        jsonnet_expr::ImportKind::Code => {
          let file = match self.file_exprs.entry(path_id) {
            Entry::Occupied(entry) => &*entry.into_mut(),
            Entry::Vacant(entry) => {
              let file = self.with_fs.get_one_file(fs, path_id)?;
              entry.insert(file.syntax.exprs)
            }
          };
          work.extend(file.imports().map(|import| (import.path, import.kind)));
        }
        jsonnet_expr::ImportKind::String => match self.import_str.entry(path_id) {
          Entry::Occupied(_) => {}
          Entry::Vacant(entry) => {
            let path = self.with_fs.artifacts.syntax.paths.get_path(path_id).to_owned();
            match fs.read_to_string(path.as_path()) {
              Ok(x) => {
                entry.insert(x);
              }
              Err(error) => return Err(PathIoError { path: path.into_path_buf(), error }),
            }
          }
        },
        jsonnet_expr::ImportKind::Binary => match self.import_bin.entry(path_id) {
          Entry::Occupied(_) => {}
          Entry::Vacant(entry) => {
            let path = self.with_fs.artifacts.syntax.paths.get_path(path_id).to_owned();
            match fs.read_to_bytes(path.as_path()) {
              Ok(x) => {
                entry.insert(x);
              }
              Err(error) => return Err(PathIoError { path: path.into_path_buf(), error }),
            }
          }
        },
      }
    }
    Ok(())
  }

  pub(crate) fn get_file_expr<F>(&mut self, fs: &F, path_id: PathId) -> Result<&jsonnet_eval::Exprs>
  where
    F: Sync + paths::FileSystem,
  {
    self.with_fs.get_file_expr(&mut self.file_exprs, fs, path_id)
  }

  pub(crate) fn get_file_artifacts<F>(
    &mut self,
    fs: &F,
    path_id: PathId,
  ) -> Result<&util::FileArtifacts>
  where
    F: Sync + paths::FileSystem,
  {
    self.with_fs.get_file_artifacts(&mut self.file_artifacts, fs, path_id)
  }
}

impl lang_srv_state::State for St {
  fn new<F>(fs: &F, val: Option<serde_json::Value>) -> Self
  where
    F: paths::FileSystem,
  {
    let mut init = util::Init::default();
    let pwd = fs.current_dir();
    let mut logger_env = env_logger::Env::default();
    // TODO is it correct to use pwd here or should we be using the root dir from the language
    // server init object (which is NOT the `val` init object passed to us here)?
    if let Ok(pwd) = &pwd {
      init.relative_to = Some(pwd.to_owned());
    }
    // TODO report errors from bad init object
    if let Some(serde_json::Value::Object(obj)) = val {
      if let Some(filter) = obj.get("log_filter").and_then(serde_json::Value::as_str) {
        if !filter.is_empty() {
          logger_env = logger_env.default_filter_or(filter.to_owned());
          init.debug = true;
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

      init.manifest = obj.get("manifest").and_then(serde_json::Value::as_bool).unwrap_or_default();
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

  fn mark_as_updated(&mut self, updated: Vec<paths::CleanPathBuf>) {
    for x in updated {
      let path_id = self.path_id(x);
      self.file_artifacts.remove(&path_id);
      self.file_exprs.remove(&path_id);
      self.import_str.remove(&path_id);
      self.import_bin.remove(&path_id);
      self.with_fs.has_errors.remove(&path_id);
    }
  }

  fn update_one<F>(
    &mut self,
    fs: &F,
    path: paths::CleanPathBuf,
    changes: Vec<apply_changes::Change>,
  ) -> (paths::PathId, Vec<diagnostic::Diagnostic>)
  where
    F: Sync + paths::FileSystem,
  {
    log::info!("update one file: {}", self.with_fs.strip(path.as_path()).display());
    let path_id = self.path_id(path.clone());
    let Some(contents) = self.open_files.get_mut(&path_id) else { return (path_id, Vec::new()) };
    apply_changes::get(contents, changes);
    self.with_fs.ensure_import_tys_cached(fs, path_id, Some(contents));
    let p = path.as_clean_path();
    let res = util::SyntaxFileToCombine::from_str(p, contents, &self.with_fs.root_dirs, fs);
    let res = res.combine(&mut self.with_fs.artifacts);
    let res = util::StaticsFileToCombine::new(res, &self.with_fs.artifacts, &self.with_fs.file_tys);
    let res = res.combine(&mut self.with_fs.artifacts);
    let wa = &self.with_fs.artifacts;
    let diagnostics = res.diagnostics(self.multi_line, &wa.statics, &wa.syntax.strings);
    let ds: Vec<_> = diagnostics.collect();
    let clean = res.is_clean();
    self.file_exprs.insert(path_id, res.syntax.exprs);
    let art = util::FileArtifacts {
      syntax: res.syntax.artifacts,
      defs: res.statics.defs,
      expr_tys: res.statics.expr_tys,
    };
    self.file_artifacts.insert(path_id, art);
    if clean {
      self.with_fs.has_errors.remove(&path_id);
    } else {
      self.with_fs.has_errors.insert(path_id);
    }
    (path_id, ds)
  }

  fn open<F>(
    &mut self,
    fs: &F,
    path: paths::CleanPathBuf,
    contents: String,
  ) -> (paths::PathId, Vec<diagnostic::Diagnostic>)
  where
    F: Sync + paths::FileSystem,
  {
    let path_id = self.path_id(path.clone());
    self.open_files.insert(path_id, contents);
    self.update_one(fs, path, Vec::new())
  }

  fn close(&mut self, path: paths::CleanPathBuf) -> PathMap<Vec<diagnostic::Diagnostic>> {
    let path_id = self.path_id(path.clone());
    self.open_files.remove(&path_id);
    std::iter::once((path_id, Vec::new())).collect()
  }

  fn is_open(&mut self, path: &paths::CleanPath) -> bool {
    let path_id = self.path_id(path.to_owned());
    self.open_files.contains_key(&path_id)
  }

  fn hover<F>(
    &mut self,
    fs: &F,
    path: paths::CleanPathBuf,
    pos: text_pos::PositionUtf16,
  ) -> Option<String>
  where
    F: Sync + paths::FileSystem,
  {
    let path_id = self.path_id(path);
    let arts = self.with_fs.get_file_artifacts(&mut self.file_artifacts, fs, path_id).ok()?;
    let tok = {
      let ts = arts.syntax.pos_db.text_size_utf16(pos)?;
      let root = arts.syntax.root.clone().into_ast()?;
      jsonnet_syntax::node_token(root.syntax(), ts)?
    };
    let node = tok.parent().and_then(|p| {
      if jsonnet_syntax::ast::Object::can_cast(p.kind()) {
        return p.parent();
      }
      if jsonnet_syntax::ast::Bind::can_cast(p.kind()) {
        let bind = jsonnet_syntax::ast::Bind::cast(p)?;
        return if let Some(p) = bind.paren_params() {
          // NOTE: this depends on us knowing that when we have a local function with the syntax
          // sugar like local f(x) = x + 1, we put the syntax node pointer on the paren params.
          Some(p.syntax().clone())
        } else {
          Some(bind.expr()?.syntax().clone())
        };
      }
      Some(p)
    });
    let expr = node.and_then(|node| {
      let ptr = jsonnet_syntax::ast::SyntaxNodePtr::new(&node);
      arts.syntax.pointers.get_idx(ptr)
    });
    let ty = expr.and_then(|expr| {
      let wa = &self.with_fs.artifacts;
      let ty = arts.expr_tys.get(&expr)?;
      let ty = ty.display(self.multi_line, &wa.statics, None, &wa.syntax.strings);
      Some(format!("type:\n```ts\n{ty}\n```"))
    });
    let from_std_field = match const_eval::get(self, fs, path_id, expr) {
      Some(const_eval::ConstEval::Std(Some(x))) => Some(x.doc()),
      Some(const_eval::ConstEval::Std(None)) => Some("std: The standard library."),
      None | Some(const_eval::ConstEval::Real(_)) => None,
    };
    let json = self.manifest.then(|| match self.get_all_deps(fs, path_id) {
      Ok(()) => match self.get_json(path_id) {
        Ok(json) => {
          let json = json.display(&self.with_fs.artifacts.syntax.strings);
          format!("json:\n```json\n{json}\n```")
        }
        Err(e) => {
          let rel_to = self.with_fs.relative_to.as_ref().map(paths::CleanPathBuf::as_clean_path);
          let a = &self.with_fs.artifacts.syntax;
          let e = e.display(&a.strings, &a.paths, rel_to);
          format!("couldn't get json: {e}")
        }
      },
      Err(e) => format!("couldn't get all deps: {e}"),
    });
    let debug = if self.debug {
      self.with_fs.get_file_expr(&mut self.file_exprs, fs, path_id).ok().map(|file| {
        let rel = self.with_fs.relative_to.as_ref().map(paths::CleanPathBuf::as_clean_path);
        let a = &self.with_fs.artifacts.syntax;
        let e = jsonnet_expr::display::expr(expr, &a.strings, &file.ar, &a.paths, rel);
        format!("debug:\n```jsonnet\n{e}\n```")
      })
    } else {
      None
    };
    let parts =
      [debug.as_deref(), json.as_deref(), ty.as_deref(), from_std_field, tok.kind().token_doc()];
    let parts: Vec<_> = parts.into_iter().flatten().collect();
    if parts.is_empty() {
      None
    } else {
      Some(parts.join("\n\n---\n\n"))
    }
  }

  fn completions<F>(
    &mut self,
    fs: &F,
    path: paths::CleanPathBuf,
    pos: text_pos::PositionUtf16,
  ) -> Option<Vec<lang_srv_state::CompletionItem>>
  where
    F: Sync + paths::FileSystem,
  {
    let path_id = self.path_id(path);
    let arts = self.with_fs.get_file_artifacts(&mut self.file_artifacts, fs, path_id).ok()?;
    let tok = {
      let ts = arts.syntax.pos_db.text_size_utf16(pos)?;
      let root = arts.syntax.root.clone().into_ast()?;
      let mut tmp = jsonnet_syntax::node_token(root.syntax(), ts)?;
      // TODO complete not just with dot
      if tmp.kind() != jsonnet_syntax::kind::SyntaxKind::Dot {
        return None;
      }
      while tmp.kind() != jsonnet_syntax::kind::SyntaxKind::Id {
        tmp = tmp.prev_token()?;
      }
      tmp
    };
    let expr = {
      let node = jsonnet_syntax::token_parent(&tok)?;
      let ptr = jsonnet_syntax::ast::SyntaxNodePtr::new(&node);
      arts.syntax.pointers.get_idx(ptr)?
    };
    let &ty = arts.expr_tys.get(&expr)?;
    let wa = &self.with_fs.artifacts;
    let fields = wa.statics.object_fields(ty)?;
    let fields = fields.iter().map(|(&name, &ty)| {
      let ty = ty.display(self.multi_line, &wa.statics, None, &wa.syntax.strings);
      lang_srv_state::CompletionItem {
        name: wa.syntax.strings.get(name).to_owned(),
        ty: ty.to_string(),
        kind: lang_srv_state::CompletionItemKind::Field,
      }
    });
    Some(fields.collect())
  }

  fn get_def<F>(
    &mut self,
    fs: &F,
    path: paths::CleanPathBuf,
    pos: text_pos::PositionUtf16,
  ) -> Option<(PathId, text_pos::RangeUtf16)>
  where
    F: Sync + paths::FileSystem,
  {
    let path_id = self.path_id(path);
    let arts = self.get_file_artifacts(fs, path_id).ok()?;
    let ce = {
      let ts = arts.syntax.pos_db.text_size_utf16(pos)?;
      let root = arts.syntax.root.clone().into_ast()?;
      let tok = jsonnet_syntax::node_token(root.syntax(), ts)?;
      let node = jsonnet_syntax::token_parent(&tok)?;
      let ptr = jsonnet_syntax::ast::SyntaxNodePtr::new(&node);
      let expr = arts.syntax.pointers.get_idx(ptr);
      let ce = const_eval::get(self, fs, path_id, expr);
      let Some(const_eval::ConstEval::Real(ce)) = ce else { return None };
      ce
    };
    let arts = self.get_file_artifacts(fs, ce.path_id).ok()?;
    // HACK: this makes go-to-def for array and object comprehensions work better. there are a few
    // reasons I can think of:
    //
    // - we desugar object comprehensions to stuff involving array comprehensions, which are then
    //   themselves also desugared
    // - we have to construct a bunch of stuff to desugar array (and object) comprehensions, and
    //   when we do that desugaring, we attach various generated identifiers to the `for`
    //   comprehension via syntax pointers.
    // - in `expr_range`, we have some extra logic to narrow the text range for those `for`
    //   comprehension variables when the def kind is a local bind, to "reverse" that desugaring
    //
    // this isn't the most elegant solution, but there are some tests that should break if this hack
    // stops working. the "worst" thing that should happen if the hack breaks is that the text range
    // reverts back to the entire comp-spec instead of the individual variable.
    //
    // or maybe something else bad that could happen is if this def kind somehow interferes with
    // stuff that isn't involved in comprehensions or local binds at all, but that seems unlikely.
    let for_comprehension_hack =
      jsonnet_expr::def::ExprDefKind::Multi(0, jsonnet_expr::def::ExprDefKindMulti::LocalBind);
    let kind = ce.kind.or(Some(for_comprehension_hack));
    let tr =
      util::expr_range(&arts.syntax.pointers, &arts.syntax.root.clone().syntax(), ce.expr, kind);
    let range = arts.syntax.pos_db.range_utf16(tr)?;
    Some((ce.path_id, range))
  }

  fn format<F>(
    &mut self,
    _: &F,
    _: paths::CleanPathBuf,
    _: u32,
  ) -> Option<(String, text_pos::PositionUtf16)>
  where
    F: Sync + paths::FileSystem,
  {
    // TODO call the external formatter process
    None
  }

  fn signature_help<F>(
    &mut self,
    fs: &F,
    path: paths::CleanPathBuf,
    pos: text_pos::PositionUtf16,
  ) -> Option<lang_srv_state::SignatureHelp>
  where
    F: Sync + paths::FileSystem,
  {
    let path_id = self.path_id(path);
    let arts = self.with_fs.get_file_artifacts(&mut self.file_artifacts, fs, path_id).ok()?;
    let tok = {
      let ts = arts.syntax.pos_db.text_size_utf16(pos)?;
      let root = arts.syntax.root.clone().into_ast()?;
      jsonnet_syntax::node_token_for_arg(root.syntax(), ts)?
    };
    let node = jsonnet_syntax::token_parent(&tok)?;
    let call = {
      let mut tmp = node.clone();
      loop {
        match jsonnet_syntax::ast::ExprCall::cast(tmp.clone()) {
          Some(x) => break x,
          None => tmp = tmp.parent()?,
        };
      }
    };
    let func_expr = {
      let func = call.expr()?;
      let ptr = jsonnet_syntax::ast::SyntaxNodePtr::new(func.syntax());
      arts.syntax.pointers.get_idx(ptr)?
    };
    let &func_ty = arts.expr_tys.get(&func_expr)?;
    let wa = &self.with_fs.artifacts;
    let func = wa.statics.as_fn(func_ty)?;
    let (label, params) = match func.display_for_sig_help(&wa.statics, None, &wa.syntax.strings) {
      Ok(x) => x,
      Err(e) => {
        always!(false, "couldn't fmt: {e}");
        return None;
      }
    };
    // use params.len() (out of bounds) when we couldn't figure it out. it seems if we send None for
    // the active param it defaults to highlighting the 0th one? but if we send out of bounds it
    // highlights nothing, which seems better.
    let active_param =
      get_cur_param(node, &tok, &call, func, &wa.syntax.strings).unwrap_or(params.len());
    Some(lang_srv_state::SignatureHelp {
      label,
      params,
      active_param: Some(always::convert::usize_to_u32(active_param)),
    })
  }

  fn paths(&self) -> &paths::Store {
    &self.with_fs.artifacts.syntax.paths
  }

  fn path_id(&mut self, path: paths::CleanPathBuf) -> PathId {
    self.with_fs.artifacts.syntax.paths.get_id_owned(path)
  }
}

fn get_cur_param(
  mut tmp: jsonnet_syntax::kind::SyntaxNode,
  tok: &jsonnet_syntax::kind::SyntaxToken,
  call: &jsonnet_syntax::ast::ExprCall,
  func: &jsonnet_ty::Fn,
  str_ar: &jsonnet_expr::StrArena,
) -> Option<usize> {
  let arg = loop {
    match jsonnet_syntax::ast::Arg::cast(tmp.clone()) {
      Some(x) => break x,
      None => match tmp.parent() {
        Some(x) => tmp = x,
        None => return matches!(tok.kind(), jsonnet_syntax::kind::SyntaxKind::LRound).then_some(0),
      },
    };
  };
  if let Some(x) = arg.id_eq() {
    let id = x.id()?;
    let text = id.text();
    let (params, _) = func.parts();
    let pos = params.iter().position(|p| {
      let s = p.id.into();
      str_ar.get(&s) == text
    })?;
    Some(pos)
  } else {
    for (idx, a) in call.args().enumerate() {
      if a.id_eq().is_some() {
        return None;
      }
      if a.syntax().text_range() == arg.syntax().text_range() {
        let pos = if tok.kind() == jsonnet_syntax::kind::SyntaxKind::Comma { idx + 1 } else { idx };
        return Some(pos);
      }
    }
    None
  }
}
