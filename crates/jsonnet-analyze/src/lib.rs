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
  pub fn get_json(&self, path_id: PathId) -> jsonnet_eval::error::Result<jsonnet_eval::Json> {
    // TODO more caching?
    let cx = jsonnet_eval::Cx {
      paths: &self.artifacts.paths,
      jsonnet_files: &self.file_exprs,
      importstr: &self.import_str,
      importbin: &self.import_bin,
      str_ar: &self.artifacts.strings,
    };
    let val = jsonnet_eval::get_exec(cx, path_id)?;
    jsonnet_eval::get_manifest(cx, val)
  }

  /// Returns the strings for this.
  #[must_use]
  pub fn strings(&self) -> &jsonnet_expr::StrArena {
    &self.artifacts.strings
  }

  fn strip<'a>(&self, p: &'a std::path::Path) -> &'a std::path::Path {
    match &self.relative_to {
      None => p,
      Some(r) => p.strip_prefix(r.as_path()).unwrap_or(p),
    }
  }

  fn display_path_id(&self, path_id: PathId) -> impl std::fmt::Display + '_ {
    self.strip(self.artifacts.paths.get_path(path_id).as_path()).display()
  }

  /// Brings all the transitive deps of the Jsonnet file with path id `path_id` into memory.
  ///
  /// It is NOT recommended to bring a massive amount of files into memory at once.
  ///
  /// # Errors
  ///
  /// When a path had an I/O error.
  pub fn get_all_deps<F>(&mut self, fs: &F, path_id: PathId) -> Result<(), PathIoError>
  where
    F: paths::FileSystem,
  {
    let mut work = vec![(path_id, jsonnet_expr::ImportKind::Code)];
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

  fn mark_as_updated(&mut self, updated: Vec<paths::CleanPathBuf>) {
    // TODO have this not take a fs and not return a PathMap?
    for x in updated {
      let path_id = self.path_id(x);
      self.file_artifacts.remove(&path_id);
      self.file_exprs.remove(&path_id);
      self.import_str.remove(&path_id);
      self.import_bin.remove(&path_id);
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
    log::info!("update one file: {}", self.strip(path.as_path()).display());
    let path_id = self.path_id(path.clone());
    let Some(contents) = self.open_files.get_mut(&path_id) else { return (path_id, Vec::new()) };
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
        return (path_id, Vec::new());
      }
    };
    let ret: Vec<_> =
      file_diagnostics(&file.errors, &file.artifacts, &self.artifacts.strings).collect();
    self.file_artifacts.insert(path_id, file.artifacts);
    self.file_exprs.insert(path_id, file.eval);
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
    _fs: &F,
    _path: paths::CleanPathBuf,
    _pos: text_pos::PositionUtf16,
  ) -> Option<String>
  where
    F: Sync + Send + paths::FileSystem,
  {
    // TODO re-impl
    None
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
      self.get_all_deps(fs, path_id).ok()?;
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
