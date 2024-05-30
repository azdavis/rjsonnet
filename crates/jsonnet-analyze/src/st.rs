//! The state of analysis.

use crate::const_eval;
use crate::util::{FileArtifacts, Init, IsolatedFile, PathIoError, Result};
use always::always;
use jsonnet_eval::JsonnetFile;
use jsonnet_syntax::ast::AstNode as _;
use paths::{PathId, PathMap};
use rustc_hash::{FxHashMap, FxHashSet};
use std::collections::hash_map::Entry;
use std::sync::OnceLock;

// TODO replace with new std lib goodies when they're in
type StdLibDoc = FxHashMap<&'static str, String>;
fn std_lib_doc() -> &'static StdLibDoc {
  static LOCK: OnceLock<StdLibDoc> = OnceLock::new();
  fn init() -> StdLibDoc {
    code_h2_md_map::get(include_str!("../../../docs/std_lib.md"), |x| format!("`std.{x}`"))
  }
  LOCK.get_or_init(init)
}

#[derive(Debug)]
struct WithFs {
  relative_to: Option<paths::CleanPathBuf>,
  root_dirs: Vec<paths::CleanPathBuf>,
  artifacts: jsonnet_expr::Artifacts,
}

impl WithFs {
  fn strip<'a>(&self, p: &'a std::path::Path) -> &'a std::path::Path {
    match &self.relative_to {
      None => p,
      Some(r) => p.strip_prefix(r.as_path()).unwrap_or(p),
    }
  }

  fn get_one_file<F>(&mut self, fs: &F, path_id: PathId) -> Result<IsolatedFile>
  where
    F: paths::FileSystem,
  {
    let path = self.artifacts.paths.get_path(path_id).to_owned();
    match IsolatedFile::from_fs(path.as_clean_path(), &self.root_dirs, &mut self.artifacts, fs) {
      Ok(x) => Ok(x),
      Err(error) => Err(PathIoError { path: path.into_path_buf(), error }),
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
  has_errors: paths::PathSet,
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
        artifacts: jsonnet_expr::Artifacts::default(),
      },
      open_files: PathMap::default(),
      file_artifacts: PathMap::default(),
      file_exprs: PathMap::default(),
      import_str: PathMap::default(),
      import_bin: PathMap::default(),
      has_errors: paths::PathSet::default(),
    }
  }

  /// Returns the json for this path.
  ///
  /// # Errors
  ///
  /// If this path couldn't be evaluated to json.
  pub fn get_json(&self, path_id: PathId) -> jsonnet_eval::error::Result<jsonnet_eval::Json> {
    if self.has_errors.contains(&path_id) {
      return Err(jsonnet_eval::error::Error::NoPath(path_id));
    }
    // TODO more caching?
    let cx = jsonnet_eval::Cx {
      paths: &self.with_fs.artifacts.paths,
      jsonnet_files: &self.file_exprs,
      importstr: &self.import_str,
      importbin: &self.import_bin,
      str_ar: &self.with_fs.artifacts.strings,
    };
    let val = jsonnet_eval::get_exec(cx, path_id)?;
    jsonnet_eval::get_manifest(cx, val)
  }

  /// Returns the strings for this.
  #[must_use]
  pub fn strings(&self) -> &jsonnet_expr::StrArena {
    &self.with_fs.artifacts.strings
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
      // warn when evaluating.
      if !seen.insert((path_id, import_kind)) {
        continue;
      }
      match import_kind {
        jsonnet_expr::ImportKind::Code => {
          let file = match self.file_exprs.entry(path_id) {
            Entry::Occupied(entry) => &*entry.into_mut(),
            Entry::Vacant(entry) => {
              let file = self.with_fs.get_one_file(fs, path_id)?;
              if !file.errors.is_empty() {
                self.has_errors.insert(path_id);
              }
              entry.insert(file.eval)
            }
          };
          work.extend(file.imports().map(|import| (import.path, import_kind)));
        }
        jsonnet_expr::ImportKind::String => match self.import_str.entry(path_id) {
          Entry::Occupied(_) => {}
          Entry::Vacant(entry) => {
            let path = self.with_fs.artifacts.paths.get_path(path_id).to_owned();
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
            let path = self.with_fs.artifacts.paths.get_path(path_id).to_owned();
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
    match self.file_exprs.entry(path_id) {
      Entry::Occupied(entry) => Ok(&*entry.into_mut()),
      Entry::Vacant(entry) => {
        let file = self.with_fs.get_one_file(fs, path_id)?;
        if !file.errors.is_empty() {
          self.has_errors.insert(path_id);
        }
        Ok(&*entry.insert(file.eval))
      }
    }
  }

  pub(crate) fn get_file_artifacts<F>(&mut self, fs: &F, path_id: PathId) -> Result<&FileArtifacts>
  where
    F: paths::FileSystem,
  {
    match self.file_artifacts.entry(path_id) {
      Entry::Occupied(entry) => Ok(&*entry.into_mut()),
      Entry::Vacant(entry) => {
        let file = self.with_fs.get_one_file(fs, path_id)?;
        if !file.errors.is_empty() {
          self.has_errors.insert(path_id);
        }
        Ok(&*entry.insert(file.artifacts))
      }
    }
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

  fn mark_as_updated(&mut self, updated: Vec<paths::CleanPathBuf>) {
    // TODO have this not take a fs and not return a PathMap?
    for x in updated {
      let path_id = self.path_id(x);
      self.file_artifacts.remove(&path_id);
      self.file_exprs.remove(&path_id);
      self.import_str.remove(&path_id);
      self.import_bin.remove(&path_id);
      self.has_errors.remove(&path_id);
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
    let file = IsolatedFile::from_str(
      path.as_clean_path(),
      contents,
      &self.with_fs.root_dirs,
      &mut self.with_fs.artifacts,
      fs,
    );
    let file = match file {
      Ok(x) => x,
      Err(e) => {
        // TODO expose a PathIoError?
        always!(false, "{}: i/o error: {}", self.with_fs.strip(path.as_path()).display(), e);
        return (path_id, Vec::new());
      }
    };
    let ret: Vec<_> = file.diagnostics(&self.with_fs.artifacts.strings).collect();
    self.file_artifacts.insert(path_id, file.artifacts);
    self.file_exprs.insert(path_id, file.eval);
    if !file.errors.is_empty() {
      self.has_errors.insert(path_id);
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
    // TODO re-impl with more
    let path_id = self.path_id(path);
    let arts = self.get_file_artifacts(fs, path_id).ok()?;
    let tok = {
      let ts = arts.pos_db.text_size_utf16(pos)?;
      let root = arts.syntax.clone().into_ast()?;
      jsonnet_syntax::node_token(root.syntax(), ts)?
    };
    // have to manually impl `and_then` because `self` is immutably borrowed up to when `arts` is
    // last used
    let std_field = match tok.parent() {
      None => None,
      Some(node) => {
        let ptr = jsonnet_syntax::ast::SyntaxNodePtr::new(&node);
        let expr = arts.pointers.get_idx(ptr);
        // TODO expose any errors here?
        const_eval::get(self, fs, path_id, expr)
      }
    };
    let from_std_field = match std_field {
      Some(const_eval::ConstEval::Std(Some(x))) => {
        std_lib_doc().get(self.with_fs.artifacts.strings.get(&x)).map(String::as_str)
      }
      Some(const_eval::ConstEval::Std(None)) => Some("The standard library."),
      None | Some(const_eval::ConstEval::Real(_)) => None,
    };
    // we can't have something be both a token and a std lib function
    from_std_field.or_else(|| tok.kind().token_doc()).map(ToOwned::to_owned)
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
      let node = tok.parent()?;
      let ptr = jsonnet_syntax::ast::SyntaxNodePtr::new(&node);
      let expr = arts.pointers.get_idx(ptr);
      // TODO expose any errors here?
      let ce = const_eval::get(self, fs, path_id, expr);
      let Some(const_eval::ConstEval::Real(ce)) = ce else { return None };
      ce
    };
    let arts = self.get_file_artifacts(fs, ce.path_id).ok()?;
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
    &self.with_fs.artifacts.paths
  }

  fn path_id(&mut self, path: paths::CleanPathBuf) -> PathId {
    self.with_fs.artifacts.paths.get_id_owned(path)
  }
}
