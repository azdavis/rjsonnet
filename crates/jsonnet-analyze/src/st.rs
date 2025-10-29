//! The state of analysis.

use crate::util::{self, PathIoError, Result};
use crate::{const_eval, format, remove};
use always::always;
use jsonnet_syntax::ast::AstNode as _;
use jsonnet_syntax::kind::{SyntaxKind, SyntaxToken};
use jsonnet_ty::display::Style;
use paths::{PathId, PathMap, PathSet};
use rayon::iter::{IntoParallelIterator as _, ParallelIterator as _};
use rustc_hash::{FxHashMap, FxHashSet};
use std::collections::hash_map::Entry;
use token::Triviable as _;

/// The part of the state that vaguely has to do with the filesystem.
///
/// But honestly, the only real reason this exists is so we can factor some things into methods
/// without getting annoying borrowing errors when shoving everything on [`St`].
///
/// Essentially, by having a separate struct and a method that takes `&mut self`, we can ensure the
/// borrow checker knows that only those fields may be affected.
#[derive(Debug)]
struct WithFs {
  root_dir: paths::CleanPathBuf,
  root_dirs: Vec<paths::CleanPathBuf>,
  artifacts: util::GlobalArtifacts,
  file_tys: paths::PathMap<jsonnet_ty::Ty>,
  /// INVARIANT: this is exactly the set of files that do have errors that have been loaded into
  /// either `file_artifacts` or `file_exprs` on the [`St`] that contains this.
  has_errors: PathSet,
  allow_unused_underscore: bool,
}

impl WithFs {
  fn strip<'a>(&self, p: &'a std::path::Path) -> &'a std::path::Path {
    p.strip_prefix(self.root_dir.as_path()).unwrap_or(p)
  }

  fn display_path_id(&self, p: PathId) -> impl std::fmt::Display {
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
    let res = util::StaticsFileToCombine::new(
      res,
      &self.artifacts,
      &self.file_tys,
      self.allow_unused_underscore,
    );
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

  /// returns a topological sort of `root_path_id` and its dependencies.
  ///
  /// the contents of `root_path_id` are taken to be `root_contents` if provided, and read from the
  /// fs if not. all other files (transitive dependencies) are always read from the fs.
  ///
  /// INVARIANTS:
  ///
  /// - `ret` will be non-empty.
  /// - `ret[0]` will be empty. (it would have contained `root_path_id` alone, but we remove it)
  /// - if exists x where `root_path_id` depends on x, then exists i in 0 ≤ i < |ret| where x in
  ///   `ret[i]`.
  /// - for all i in 0 ≤ i < |ret|, for all x in `ret[i]`, if exists y where x depends on y, then
  ///   exists j in i < j ≤ |ret| where y in `ret[j]`.
  ///
  /// this means that:
  ///
  /// - the "leaves" are at the end of the returned vec.
  /// - processing the vec sequentially in **reverse** order will mean that when we process a path,
  ///   we have already proceeded all of its dependencies.
  /// - it is possible to parallelize processing over each set in the vec, since no files in each
  ///   set depend on one another.
  fn topological_sort<F>(
    &mut self,
    fs: &F,
    root_path_id: PathId,
    root_contents: Option<&str>,
  ) -> Vec<PathSet>
  where
    F: Sync + paths::FileSystem,
  {
    self.file_tys.remove(&root_path_id);
    let mut work = topo_sort::Work::<PathId>::default();
    work.push(root_path_id);
    let mut visitor =
      TopoSortVisitor { with_fs: self, fs, root_path_id, root_contents, ret: Vec::new() };
    let got = work.run(&mut visitor);
    let mut ret = visitor.finish();
    let done = got.done;
    if cfg!(debug_assertions) {
      let all_levels: PathSet = ret.iter().flatten().copied().collect();
      assert_eq!(all_levels, done, "everything done should be in the levels");
      let all_levels_count: usize = ret.iter().map(PathSet::len).sum();
      assert_eq!(all_levels_count, done.len(), "should have no duplicates across levels");
    }
    if let Some(fst) = ret.first_mut() {
      always!(fst.remove(&root_path_id), "root should be in first level");
      always!(fst.is_empty(), "only root should be in first level");
    } else {
      always!(false, "should have a first level");
    }
    ret
  }

  /// ensures the transitive dependencies of `orig_path_id` have their type info loaded into the
  /// cache.
  fn ensure_import_tys_cached<F>(&mut self, fs: &F, orig_path_id: PathId, contents: Option<&str>)
  where
    F: Sync + paths::FileSystem,
  {
    log::info!(
      "ensure_import_tys_cached {:?} {}",
      orig_path_id,
      self.display_path_id(orig_path_id)
    );
    let levels = self.topological_sort(fs, orig_path_id, contents);
    log::info!("levels: {levels:?}");
    // TODO allow asking for less analysis to just get the types, not the diagnostics
    for level in levels.into_iter().rev() {
      // parallel
      let syntax_files = level.into_par_iter().filter_map(|path_id| {
        let path = self.artifacts.syntax.paths.get_path(path_id);
        let res = util::SyntaxFileToCombine::from_fs(path, &self.root_dirs, fs).ok()?;
        Some((path_id, res))
      });
      // unzip so we don't have to carry around the path_id unchanged in the next few
      // transformations, in which order will be preserved.
      let (order, syntax_files): (Vec<_>, Vec<_>) = syntax_files.unzip();
      // sequential
      let syntax_files = syntax_files.into_iter().map(|res| res.combine(&mut self.artifacts));
      let syntax_files: Vec<_> = syntax_files.collect();
      // parallel
      let statics_files = syntax_files.into_par_iter().map(|res| {
        util::StaticsFileToCombine::new(
          res,
          &self.artifacts,
          &self.file_tys,
          self.allow_unused_underscore,
        )
      });
      let statics_files: Vec<_> = statics_files.collect();
      always!(order.len() == statics_files.len());
      // sequential
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

struct TopoSortVisitor<'a, F> {
  with_fs: &'a mut WithFs,
  fs: &'a F,
  root_path_id: PathId,
  root_contents: Option<&'a str>,
  ret: Vec<PathSet>,
}

impl<F> TopoSortVisitor<'_, F> {
  fn finish(self) -> Vec<PathSet> {
    self.ret
  }
}

impl<F> topo_sort::Visitor for TopoSortVisitor<'_, F>
where
  F: Sync + paths::FileSystem,
{
  type Elem = PathId;

  type Data = ();

  type Set = PathSet;

  fn enter(&self, elem: PathId) -> Option<Self::Data> {
    if self.with_fs.file_tys.contains_key(&elem) { None } else { Some(()) }
  }

  fn process(&mut self, path_id: PathId, (): Self::Data, work: &mut topo_sort::Work<PathId>) {
    let path = self.with_fs.artifacts.syntax.paths.get_path(path_id);
    let parent = util::path_parent_must(path);
    let fs_contents: String;
    let contents = match self.root_contents.take() {
      Some(x) => {
        always!(path_id == self.root_path_id);
        x
      }
      None => match self.fs.read_to_string(path.as_path()) {
        Ok(x) => {
          fs_contents = x;
          fs_contents.as_str()
        }
        Err(e) => {
          log::warn!("io error: {e}");
          return;
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
      let dirs = jsonnet_resolve_import::NonEmptyDirs::new(parent, &self.with_fs.root_dirs);
      let import = jsonnet_resolve_import::get(import, dirs.iter(), &util::FsAdapter(self.fs));
      let Some(import) = import else { continue };
      log::debug!("new import: {}", import.as_clean_path().as_path().display());
      // ...because we mutate `paths` here.
      let import = self.with_fs.artifacts.syntax.paths.get_id_owned(import);
      // this mutation also makes it too annoying to write this for-push as a
      // iter-filter-map-extend.
      work.push(import);
    }
  }

  fn exit(&mut self, path_id: PathId, level_idx: usize) {
    if level_idx >= self.ret.len() {
      self.ret.resize_with(level_idx + 1, PathSet::default);
    }
    let Some(level) = self.ret.get_mut(level_idx) else {
      always!(false, "`ret` should have been resized to at least `{}`", level_idx + 1);
      return;
    };
    level.insert(path_id);
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
  style: Style,
  format_engine: Option<util::FormatEngine>,
}

impl St {
  /// Returns a new `St` with the given init options.
  #[must_use]
  pub fn init(root_dir: paths::CleanPathBuf, init: util::Init) -> Self {
    log::info!("make new St with {init:?}");
    Self {
      with_fs: WithFs {
        root_dir,
        root_dirs: init.root_dirs,
        artifacts: util::GlobalArtifacts::default(),
        file_tys: paths::PathMap::default(),
        has_errors: PathSet::default(),
        allow_unused_underscore: init.allow_unused_underscore,
      },
      open_files: PathMap::default(),
      file_artifacts: PathMap::default(),
      file_exprs: PathMap::default(),
      import_str: PathMap::default(),
      import_bin: PathMap::default(),
      manifest: init.manifest,
      debug: init.debug,
      style: init.style,
      format_engine: init.format_engine,
    }
  }

  /// Returns the json for this path.
  ///
  /// # Errors
  ///
  /// If this path couldn't be evaluated to json.
  pub fn get_json(
    &mut self,
    path_id: PathId,
  ) -> jsonnet_eval::error::Result<jsonnet_val::json::Val> {
    if self.with_fs.has_errors.contains(&path_id) {
      return Err(jsonnet_eval::error::Error::HasErrors(path_id));
    }
    let mut cx = jsonnet_eval::Cx {
      paths: &self.with_fs.artifacts.syntax.paths,
      str_ar: &mut self.with_fs.artifacts.syntax.strings,
      exprs: &mut self.file_exprs,
      import_str: &self.import_str,
      import_bin: &self.import_bin,
      obj_mk: jsonnet_val::jsonnet::ObjectMk::default(),
    };
    let val = jsonnet_eval::get_exec(&mut cx, path_id)?;
    jsonnet_eval::get_manifest(&mut cx, val)
  }

  /// Returns the strings for this.
  #[must_use]
  pub fn strings(&self) -> &jsonnet_expr::StrArena {
    &self.with_fs.artifacts.syntax.strings
  }

  /// Returns the mutable strings for this.
  #[must_use]
  pub fn strings_mut(&mut self) -> &mut jsonnet_expr::StrArena {
    &mut self.with_fs.artifacts.syntax.strings
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

  /// Removes unused vars.
  pub fn remove_unused<F>(
    &mut self,
    fs: &F,
    path: &paths::CleanPath,
    options: remove::Options,
  ) -> Option<String>
  where
    F: Sync + paths::FileSystem,
  {
    let path_id = self.with_fs.artifacts.syntax.paths.get_id(path);
    let contents = self.open_files.get(&path_id)?;
    let f = util::SyntaxFileToCombine::from_str(path, contents, &self.with_fs.root_dirs, fs);
    let f = f.combine(&mut self.with_fs.artifacts);
    if !f.errors.is_empty() {
      return None;
    }
    // NOTE: no need to check for import deps, as unused var errors are not affected by imports
    let f = util::StaticsFileToCombine::new(
      f,
      &self.with_fs.artifacts,
      &self.with_fs.file_tys,
      self.with_fs.allow_unused_underscore,
    );
    let f = f.combine(&mut self.with_fs.artifacts);
    let root_syntax = f.syntax.artifacts.root.clone().syntax();
    let local_to_binds = {
      let ar = &f.syntax.exprs.ar;
      let mut tmp = FxHashMap::<jsonnet_syntax::ast::SyntaxNodePtr, FxHashSet<usize>>::default();
      for err in f.statics.errors {
        let Some((expr, _, idx)) = err.into_unused_local() else { continue };
        let jsonnet_expr::ExprData::Local { binds, .. } = &ar[expr] else { continue };
        let Some(&(_, Some(rhs))) = binds.get(idx) else { continue };
        match (options.flavor, &ar[rhs]) {
          (remove::Flavor::Imports, jsonnet_expr::ExprData::Import { .. })
          | (remove::Flavor::All, _) => {}
          (remove::Flavor::Imports, _) => continue,
        }
        let Some(ptr) = f.syntax.artifacts.pointers.get_ptr(expr) else { continue };
        tmp.entry(ptr).or_default().insert(idx);
      }
      tmp
    };
    let ranges = {
      let mut tmp = FxHashSet::<text_size::TextRange>::default();
      for (ptr, binds) in &local_to_binds {
        let Some(node) = ptr.try_to_node(&root_syntax) else { continue };
        // TODO for object locals, this is an Object, not an ExprLocal
        let Some(local) = jsonnet_syntax::ast::ExprLocal::cast(node) else { continue };
        let total_binds = local.bind_commas().count();
        if binds.len() == total_binds && (0..total_binds).all(|x| binds.contains(&x)) {
          let start = local.local_kw();
          let end = local.semicolon();
          let Some(range) = remove::trivia_around(start, end, options.comments) else { continue };
          tmp.insert(range);
        } else {
          for &idx in binds {
            let Some(bind) = local.bind_commas().nth(idx) else { continue };
            let prev_comma = if bind.comma().is_some() {
              None
            } else {
              idx.checked_sub(1).and_then(|idx| {
                if binds.contains(&idx) {
                  return None;
                }
                local.bind_commas().nth(idx)?.comma()
              })
            };
            let start = prev_comma.or_else(|| bind.syntax().first_token());
            let end = bind.syntax().last_token();
            let Some(range) = remove::trivia_around(start, end, options.comments) else { continue };
            tmp.insert(range);
          }
        }
      }
      tmp
    };
    if ranges.is_empty() {
      return None;
    }
    let mut tok = root_syntax.first_token()?;
    let mut ret = String::new();
    let mut prev_alpha_numeric = false;
    let mut gap = None::<Gap>;
    loop {
      let tok_range = tok.text_range();
      let this_alpha_numeric = is_alpha_numeric(tok.kind());
      if ranges.iter().any(|r| r.contains_range(tok_range)) {
        let has_nl = tok.kind().is_trivia() && tok.text().contains('\n');
        let g = if has_nl { Gap::Newline } else { Gap::Space };
        gap = Some(gap.map_or(g, |x| x.and(g)));
      } else {
        if ((prev_alpha_numeric && this_alpha_numeric) || gap.is_some_and(Gap::is_newline))
          && let Some(gap) = gap
        {
          ret.push(gap.as_char());
        }
        ret.push_str(tok.text());
        gap = None;
        prev_alpha_numeric = this_alpha_numeric;
      }
      tok = match tok.next_token() {
        Some(x) => x,
        None => break,
      };
    }
    Some(ret)
  }
}

impl lang_srv_state::State for St {
  fn new(root_dir: paths::CleanPathBuf, val: Option<serde_json::Value>) -> Self {
    let mut init = util::Init { allow_unused_underscore: true, ..Default::default() };
    let mut logger_env = env_logger::Env::default();
    if let Some(serde_json::Value::Object(obj)) = val {
      if let Some(filter) = obj.get("log_filter").and_then(serde_json::Value::as_str)
        && !filter.is_empty()
      {
        logger_env = logger_env.default_filter_or(filter.to_owned());
        init.debug = true;
      }

      init.root_dirs = obj
        .get("root_dirs")
        .and_then(|x| {
          let ary = x.as_array()?;
          ary
            .iter()
            .map(|x| x.as_str().map(|x| root_dir.as_clean_path().join(x)))
            .collect::<Option<Vec<_>>>()
        })
        .unwrap_or_default();

      init.manifest = obj.get("manifest").and_then(serde_json::Value::as_bool).unwrap_or_default();

      init.format_engine = obj.get("format_engine").and_then(|x| {
        let s = x.as_str()?;
        if s == "none" {
          return None;
        }
        s.parse().ok()
      });
    }

    if let Err(e) = env_logger::try_init_from_env(logger_env) {
      always!(false, "couldn't init logger: {e}");
    }

    Self::init(root_dir, init)
  }

  const BUG_REPORT_MSG: &'static str =
    "please file a bug report: https://github.com/azdavis/rjsonnet/issues";

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
  ) -> (PathId, Vec<diagnostic::Diagnostic>)
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
    let res = util::StaticsFileToCombine::new(
      res,
      &self.with_fs.artifacts,
      &self.with_fs.file_tys,
      self.with_fs.allow_unused_underscore,
    );
    let res = res.combine(&mut self.with_fs.artifacts);
    let wa = &self.with_fs.artifacts;
    let diagnostics = res.diagnostics(self.style, &wa.statics, &wa.syntax.strings);
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
  ) -> (PathId, Vec<diagnostic::Diagnostic>)
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
    if tok.kind().is_trivia() {
      return None;
    }
    let (node, param_pos) = match tok.parent() {
      None => (None, None),
      Some(node) => {
        if jsonnet_syntax::ast::Object::can_cast(node.kind()) {
          (node.parent(), None)
        } else if jsonnet_syntax::ast::Param::can_cast(node.kind()) {
          let parent = node.parent();
          let pos = parent.as_ref().and_then(|parent| {
            parent.children().position(|child| child.text_range() == node.text_range())
          });
          (parent, pos)
        } else if let Some(bind) = jsonnet_syntax::ast::Bind::cast(node.clone()) {
          if let Some(paren_params) = bind.paren_params() {
            // NOTE: this depends on us knowing that when we have a local function with the syntax
            // sugar like local f(x) = x + 1, we put the syntax node pointer on the paren params.
            (Some(paren_params.syntax().clone()), None)
          } else {
            (bind.expr().map(|e| e.syntax().clone()), None)
          }
        } else {
          (Some(node), None)
        }
      }
    };
    // need this because we attach stuff to the syntax node pointer for paren params for desugared
    // functions, but we do NOT do that for functions that were actually functions in the concrete
    // syntax.
    let paren_params_parent = node.as_ref().and_then(|x| {
      if jsonnet_syntax::ast::ParenParams::can_cast(x.kind()) { x.parent() } else { None }
    });
    let expr = [node, paren_params_parent].into_iter().flatten().find_map(|node| {
      let ptr = jsonnet_syntax::ast::SyntaxNodePtr::new(&node);
      arts.syntax.pointers.get_idx(ptr)
    });
    let ty = expr.and_then(|expr| {
      let wa = &self.with_fs.artifacts;
      let &ty = arts.expr_tys.get(&expr)?;
      let ty = param_pos
        .and_then(|pos| {
          let func = wa.statics.as_fn(ty)?;
          let (params, _) = func.parts()?;
          let param = params.get(pos)?;
          Some(param.ty)
        })
        .unwrap_or(ty);
      let ty = ty.display(self.style, &wa.statics, None, &wa.syntax.strings);
      Some(format!("type:\n```jsonnet-ty\n{ty}\n```"))
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
          let rel = self.with_fs.root_dir.as_clean_path();
          let a = &self.with_fs.artifacts.syntax;
          let e = e.display(&a.strings, &a.paths, Some(rel));
          format!("couldn't get json: {e}")
        }
      },
      Err(e) => format!("couldn't get all deps: {e}"),
    });
    let debug = if self.debug {
      self.with_fs.get_file_expr(&mut self.file_exprs, fs, path_id).ok().map(|file| {
        let rel = Some(self.with_fs.root_dir.as_clean_path());
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
    if parts.is_empty() { None } else { Some(parts.join("\n\n---\n\n")) }
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
    let id_tok = {
      let ts = arts.syntax.pos_db.text_size_utf16(pos)?;
      let root = arts.syntax.root.clone().into_ast()?;
      let cur = jsonnet_syntax::node_token(root.syntax(), ts)?;
      let prev = non_trivia(cur.prev_token()?)?;
      let tmp = match cur.kind() {
        SyntaxKind::Dot => prev,
        SyntaxKind::Id => {
          if prev.kind() != SyntaxKind::Dot {
            return None;
          }
          non_trivia(prev.prev_token()?)?
        }
        _ => return None,
      };
      if tmp.kind() != SyntaxKind::Id {
        return None;
      }
      tmp
    };
    let dot_tok_range_start = {
      let mut dot_tok = id_tok.next_token()?;
      while dot_tok.kind() != SyntaxKind::Dot {
        dot_tok = dot_tok.next_token()?;
      }
      arts.syntax.pos_db.range_utf16(dot_tok.text_range())?.start
    };
    let expr = {
      let node = jsonnet_syntax::token_parent(&id_tok)?;
      let ptr = jsonnet_syntax::ast::SyntaxNodePtr::new(&node);
      arts.syntax.pointers.get_idx(ptr)?
    };
    let &ty = arts.expr_tys.get(&expr)?;
    let expr_is_std = matches!(
      const_eval::get(self, fs, path_id, Some(expr)),
      Some(const_eval::ConstEval::Std(None))
    );
    let fields = self.with_fs.artifacts.statics.known_fields(ty)?;
    let wa = &self.with_fs.artifacts;
    let fields = fields.into_iter().map(|(name, ty)| {
      let doc = if expr_is_std {
        jsonnet_expr::StdField::try_from(name).ok().map(|x| x.doc().to_owned())
      } else {
        None
      };
      let name = wa.syntax.strings.get(name);
      let ty = ty.display(self.style, &wa.statics, None, &wa.syntax.strings);
      let (text_edit, additional_text_edits) = if jsonnet_ident::is(name) {
        (None, None)
      } else {
        let unescape = jsonnet_escape::Unescape::new(name);
        let main_edit = lang_srv_state::TextEdit {
          text: format!("[{unescape}]"),
          range: text_pos::RangeUtf16::zero(pos),
        };
        let remove_until_open_bracket = lang_srv_state::TextEdit {
          text: String::new(),
          range: text_pos::RangeUtf16 { start: dot_tok_range_start, end: pos },
        };
        (Some(main_edit), Some(vec![remove_until_open_bracket]))
      };
      lang_srv_state::CompletionItem {
        name: name.to_owned(),
        ty: ty.to_string(),
        kind: lang_srv_state::CompletionItemKind::Field,
        doc,
        text_edit,
        additional_text_edits,
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
    path: paths::CleanPathBuf,
    _: u32,
  ) -> Option<(String, text_pos::PositionUtf16)>
  where
    F: Sync + paths::FileSystem,
  {
    let engine = self.format_engine?;
    let path_id = self.with_fs.artifacts.syntax.paths.get_id(path.as_clean_path());
    let contents = self.open_files.get(&path_id)?;
    let fmt_res =
      format::get(engine, self.with_fs.root_dir.as_clean_path(), path.as_clean_path(), contents);
    match fmt_res {
      Ok(x) => {
        let pos = text_pos::PositionDb::new(contents.as_str()).end_position_utf16();
        Some((x, pos))
      }
      Err(e) => {
        log::error!("format failed: {e}");
        None
      }
    }
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
      let tmp = jsonnet_syntax::node_token(root.syntax(), ts)?;
      non_trivia(tmp)?
    };
    let node = jsonnet_syntax::token_parent(&tok)?;
    let call = {
      let mut tmp = node.clone();
      loop {
        match jsonnet_syntax::ast::ExprCall::cast(tmp.clone()) {
          Some(x) => break x,
          None => tmp = tmp.parent()?,
        }
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
  mut arg: jsonnet_syntax::kind::SyntaxNode,
  tok: &jsonnet_syntax::kind::SyntaxToken,
  call: &jsonnet_syntax::ast::ExprCall,
  func: &jsonnet_ty::Fn,
  str_ar: &jsonnet_expr::StrArena,
) -> Option<usize> {
  let arg = loop {
    match jsonnet_syntax::ast::Arg::cast(arg.clone()) {
      Some(x) => break x,
      None => match arg.parent() {
        Some(x) => arg = x,
        None => return matches!(tok.kind(), SyntaxKind::LRound).then_some(0),
      },
    }
  };
  if let Some(x) = arg.id_eq() {
    let id = x.id()?;
    let text = id.text();
    let (params, _) = func.parts()?;
    let pos = params.iter().position(|p| str_ar.get_id(p.id) == Some(text))?;
    Some(pos)
  } else {
    for (idx, a) in call.args().enumerate() {
      if a.id_eq().is_some() {
        return None;
      }
      if a.syntax().text_range() == arg.syntax().text_range() {
        let pos = if tok.kind() == SyntaxKind::Comma { idx + 1 } else { idx };
        return Some(pos);
      }
    }
    None
  }
}

fn non_trivia(mut ret: SyntaxToken) -> Option<SyntaxToken> {
  while ret.kind().is_trivia() {
    ret = ret.prev_token()?;
  }
  Some(ret)
}

#[derive(Debug, Clone, Copy)]
enum Gap {
  Space,
  Newline,
}

impl Gap {
  fn as_char(self) -> char {
    match self {
      Gap::Space => ' ',
      Gap::Newline => '\n',
    }
  }

  fn and(self, other: Self) -> Self {
    match (self, other) {
      (Gap::Newline, _) | (_, Gap::Newline) => Gap::Newline,
      (Gap::Space, Gap::Space) => Gap::Space,
    }
  }

  fn is_newline(self) -> bool {
    matches!(self, Gap::Newline)
  }
}

fn is_alpha_numeric(kind: SyntaxKind) -> bool {
  matches!(
    kind,
    SyntaxKind::Id
      | SyntaxKind::Number
      | SyntaxKind::ImportbinKw
      | SyntaxKind::ImportstrKw
      | SyntaxKind::FunctionKw
      | SyntaxKind::AssertKw
      | SyntaxKind::ImportKw
      | SyntaxKind::ErrorKw
      | SyntaxKind::FalseKw
      | SyntaxKind::LocalKw
      | SyntaxKind::SuperKw
      | SyntaxKind::ElseKw
      | SyntaxKind::NullKw
      | SyntaxKind::SelfKw
      | SyntaxKind::ThenKw
      | SyntaxKind::TrueKw
      | SyntaxKind::ForKw
      | SyntaxKind::IfKw
      | SyntaxKind::InKw
  )
}
