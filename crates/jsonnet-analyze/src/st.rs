//! The state of analysis.

use crate::util::{
  FileArtifacts, GlobalArtifacts, Init, IsolatedArtifacts, IsolatedFile, PathIoError, Result,
};
use crate::{const_eval, util};
use always::always;
use jsonnet_eval::JsonnetFile;
use jsonnet_syntax::ast::AstNode as _;
use paths::{PathId, PathMap};
use rustc_hash::{FxHashMap, FxHashSet};
use std::collections::hash_map::Entry;
use std::sync::LazyLock;

static STD_LIB_DOC: LazyLock<FxHashMap<&str, String>> = LazyLock::new(|| {
  code_h2_md_map::get(include_str!("../../../docs/std_lib.md"), |x| format!("std item: `{x}`"))
});

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
  file_tys: paths::PathMap<jsonnet_statics::ty::Ty>,
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

  /// useful for debugging, so let's keep it around.
  #[allow(dead_code)]
  fn display_path_id(&self, p: PathId) -> impl std::fmt::Display + '_ {
    self.strip(self.artifacts.expr.paths.get_path(p).as_path()).display()
  }

  fn get_one_file<F>(&mut self, fs: &F, path_id: PathId) -> Result<IsolatedFile>
  where
    F: paths::FileSystem,
  {
    let path = self.artifacts.expr.paths.get_path(path_id);
    let art =
      IsolatedArtifacts::from_fs(path, &self.root_dirs, &self.artifacts, &self.file_tys, fs);
    match art {
      Ok(art) => {
        let file = art.combine(&mut self.artifacts);
        if file.errors.is_empty() {
          self.has_errors.remove(&path_id);
        } else {
          self.has_errors.insert(path_id);
        }
        Ok(file)
      }
      Err(error) => Err(PathIoError { path: path.to_owned().into_path_buf(), error }),
    }
  }

  fn get_file_expr<'fe, F>(
    &mut self,
    file_exprs: &'fe mut PathMap<JsonnetFile>,
    fs: &F,
    path_id: PathId,
  ) -> Result<&'fe JsonnetFile>
  where
    F: paths::FileSystem,
  {
    match file_exprs.entry(path_id) {
      Entry::Occupied(entry) => Ok(&*entry.into_mut()),
      Entry::Vacant(entry) => {
        let file = self.get_one_file(fs, path_id)?;
        Ok(&*entry.insert(file.eval))
      }
    }
  }

  fn get_file_artifacts<'fa, F>(
    &mut self,
    file_artifacts: &'fa mut PathMap<FileArtifacts>,
    fs: &F,
    path_id: PathId,
  ) -> Result<&'fa FileArtifacts>
  where
    F: paths::FileSystem,
  {
    match file_artifacts.entry(path_id) {
      Entry::Occupied(entry) => Ok(&*entry.into_mut()),
      Entry::Vacant(entry) => {
        let file = self.get_one_file(fs, path_id)?;
        Ok(&*entry.insert(file.artifacts))
      }
    }
  }
}

/// The state of analysis.
#[derive(Debug)]
pub struct St {
  with_fs: WithFs,
  open_files: PathMap<String>,
  file_artifacts: PathMap<FileArtifacts>,
  file_exprs: PathMap<JsonnetFile>,
  import_str: PathMap<String>,
  import_bin: PathMap<Vec<u8>>,
  manifest: bool,
  debug: bool,
}

impl St {
  /// Returns a new `St` with the given init options.
  #[must_use]
  pub fn init(init: Init) -> Self {
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
    }
  }

  /// Returns the json for this path.
  ///
  /// # Errors
  ///
  /// If this path couldn't be evaluated to json.
  pub fn get_json(&self, path_id: PathId) -> jsonnet_eval::error::Result<jsonnet_eval::Json> {
    if self.with_fs.has_errors.contains(&path_id) {
      return Err(jsonnet_eval::error::Error::NoPath(path_id));
    }
    // TODO more caching?
    let cx = jsonnet_eval::Cx {
      paths: &self.with_fs.artifacts.expr.paths,
      str_ar: &self.with_fs.artifacts.expr.strings,
      jsonnet_files: &self.file_exprs,
      import_str: &self.import_str,
      import_bin: &self.import_bin,
    };
    let val = jsonnet_eval::get_exec(cx, path_id)?;
    jsonnet_eval::get_manifest(cx, val)
  }

  /// Returns the strings for this.
  #[must_use]
  pub fn strings(&self) -> &jsonnet_expr::StrArena {
    &self.with_fs.artifacts.expr.strings
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
    F: paths::FileSystem,
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
              entry.insert(file.eval)
            }
          };
          work.extend(file.imports().map(|import| (import.path, import.kind)));
        }
        jsonnet_expr::ImportKind::String => match self.import_str.entry(path_id) {
          Entry::Occupied(_) => {}
          Entry::Vacant(entry) => {
            let path = self.with_fs.artifacts.expr.paths.get_path(path_id).to_owned();
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
            let path = self.with_fs.artifacts.expr.paths.get_path(path_id).to_owned();
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

  pub(crate) fn get_file_expr<F>(&mut self, fs: &F, path_id: PathId) -> Result<&JsonnetFile>
  where
    F: paths::FileSystem,
  {
    self.with_fs.get_file_expr(&mut self.file_exprs, fs, path_id)
  }

  pub(crate) fn get_file_artifacts<F>(&mut self, fs: &F, path_id: PathId) -> Result<&FileArtifacts>
  where
    F: paths::FileSystem,
  {
    self.with_fs.get_file_artifacts(&mut self.file_artifacts, fs, path_id)
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
    F: Sync + Send + paths::FileSystem,
  {
    log::info!("update one file: {}", self.with_fs.strip(path.as_path()).display());
    let path_id = self.path_id(path.clone());
    let Some(contents) = self.open_files.get_mut(&path_id) else { return (path_id, Vec::new()) };
    apply_changes::get(contents, changes);
    let art = IsolatedArtifacts::from_str(
      path.as_clean_path(),
      contents,
      &self.with_fs.root_dirs,
      &self.with_fs.artifacts,
      &self.with_fs.file_tys,
      fs,
    );
    let art = match art {
      Ok(x) => x,
      Err(e) => {
        // TODO expose a PathIoError?
        always!(false, "{}: i/o error: {}", self.with_fs.strip(path.as_path()).display(), e);
        return (path_id, Vec::new());
      }
    };
    let file = art.combine(&mut self.with_fs.artifacts);
    let root = file.artifacts.syntax.clone();
    let syntax = root.syntax();
    let diagnostics =
      file.diagnostics(&syntax, &self.with_fs.artifacts.tys, &self.with_fs.artifacts.expr.strings);
    let ret: Vec<_> = diagnostics.collect();
    self.file_artifacts.insert(path_id, file.artifacts);
    self.file_exprs.insert(path_id, file.eval);
    if file.errors.is_empty() {
      self.with_fs.has_errors.remove(&path_id);
    } else {
      self.with_fs.has_errors.insert(path_id);
    }
    (path_id, ret)
  }

  /// Opens a path.
  #[must_use]
  fn open<F>(
    &mut self,
    fs: &F,
    path: paths::CleanPathBuf,
    contents: String,
  ) -> (paths::PathId, Vec<diagnostic::Diagnostic>)
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
    fs: &F,
    path: paths::CleanPathBuf,
    pos: text_pos::PositionUtf16,
  ) -> Option<String>
  where
    F: Sync + Send + paths::FileSystem,
  {
    let path_id = self.path_id(path);
    let arts = self.with_fs.get_file_artifacts(&mut self.file_artifacts, fs, path_id).ok()?;
    let tok = {
      let ts = arts.pos_db.text_size_utf16(pos)?;
      let root = arts.syntax.clone().into_ast()?;
      jsonnet_syntax::node_token(root.syntax(), ts)?
    };
    let expr = jsonnet_syntax::token_parent(&tok).and_then(|node| {
      let ptr = jsonnet_syntax::ast::SyntaxNodePtr::new(&node);
      arts.pointers.get_idx(ptr)
    });
    let ty = expr.and_then(|expr| {
      let ty = arts.expr_tys.get(&expr)?;
      let ty = ty.display(&self.with_fs.artifacts.tys, &self.with_fs.artifacts.expr.strings);
      Some(format!("type:\n```ts\n{ty}\n```"))
    });
    // TODO expose any errors here?
    let from_std_field = match const_eval::get(self, fs, path_id, expr) {
      Some(const_eval::ConstEval::Std(Some(x))) => {
        STD_LIB_DOC.get(self.with_fs.artifacts.expr.strings.get(&x)).map(String::as_str)
      }
      Some(const_eval::ConstEval::Std(None)) => Some("std: The standard library."),
      None | Some(const_eval::ConstEval::Real(_)) => None,
    };
    let json = self.manifest.then(|| match self.get_all_deps(fs, path_id) {
      Ok(()) => match self.get_json(path_id) {
        Ok(json) => {
          let json = json.display(&self.with_fs.artifacts.expr.strings);
          format!("json:\n```json\n{json}\n```")
        }
        Err(e) => {
          let rel_to = self.with_fs.relative_to.as_ref().map(paths::CleanPathBuf::as_clean_path);
          let a = &self.with_fs.artifacts.expr;
          let e = e.display(&a.strings, &a.paths, rel_to);
          format!("couldn't get json: {e}")
        }
      },
      Err(e) => format!("couldn't get all deps: {e}"),
    });
    let debug = if self.debug {
      self.with_fs.get_file_expr(&mut self.file_exprs, fs, path_id).ok().map(|file| {
        let rel = self.with_fs.relative_to.as_ref().map(paths::CleanPathBuf::as_clean_path);
        let a = &self.with_fs.artifacts.expr;
        let e = jsonnet_expr::display::expr(expr, &a.strings, &file.expr_ar, &a.paths, rel);
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
      let arts = self.get_file_artifacts(fs, path_id).ok()?;
      let ts = arts.pos_db.text_size_utf16(pos)?;
      let root = arts.syntax.clone().into_ast()?;
      let tok = jsonnet_syntax::node_token(root.syntax(), ts)?;
      let node = jsonnet_syntax::token_parent(&tok)?;
      let ptr = jsonnet_syntax::ast::SyntaxNodePtr::new(&node);
      let expr = arts.pointers.get_idx(ptr);
      // TODO expose any errors here?
      let ce = const_eval::get(self, fs, path_id, expr);
      let Some(const_eval::ConstEval::Real(ce)) = ce else { return None };
      ce
    };
    let arts = self.get_file_artifacts(fs, ce.path_id).ok()?;
    let root = arts.syntax.clone().into_ast()?;
    let tr = util::expr_range(&arts.pointers, root.syntax(), ce.expr, ce.kind);
    let range = arts.pos_db.range_utf16(tr)?;
    Some((ce.path_id, range))
  }

  fn paths(&self) -> &paths::Store {
    &self.with_fs.artifacts.expr.paths
  }

  fn path_id(&mut self, path: paths::CleanPathBuf) -> PathId {
    self.with_fs.artifacts.expr.paths.get_id_owned(path)
  }
}
