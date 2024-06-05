//! Utilities.

use always::always;
use diagnostic::Diagnostic;
use jsonnet_eval::JsonnetFile;
use jsonnet_expr::def;
use std::fmt;

/// Options for initialization.
#[derive(Debug, Default)]
pub struct Init {
  /// Path to which other paths may be displayed relative.
  pub relative_to: Option<paths::CleanPathBuf>,
  /// Extra directories in which to search for import paths.
  pub root_dirs: Vec<paths::CleanPathBuf>,
  /// Whether to manifest into JSON.
  ///
  /// Might be slow when enabled.
  pub manifest: bool,
  /// Whether to output extra debug info.
  pub debug: bool,
}

/// Artifacts from a file whose shared artifacts have been combined into the global ones.
#[derive(Debug)]
pub(crate) struct FileArtifacts {
  pub(crate) pos_db: text_pos::PositionDb,
  pub(crate) syntax: jsonnet_syntax::Root,
  pub(crate) pointers: jsonnet_desugar::Pointers,
  pub(crate) defs: jsonnet_expr::def::Map,
  /// TODO have one global ty store?
  pub(crate) tys: jsonnet_statics::ty::Store,
  pub(crate) expr_tys: jsonnet_statics::ty::Exprs,
}

/// Errors from a file analyzed in isolation.
#[derive(Debug, Default)]
pub(crate) struct FileErrors {
  pub(crate) lex: Vec<jsonnet_lex::Error>,
  pub(crate) parse: Vec<jsonnet_parse::Error>,
  pub(crate) desugar: Vec<jsonnet_desugar::Error>,
  pub(crate) statics: Vec<jsonnet_statics::error::Error>,
}

impl FileErrors {
  pub(crate) fn is_empty(&self) -> bool {
    self.lex.is_empty()
      && self.parse.is_empty()
      && self.desugar.is_empty()
      && self.statics.is_empty()
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

/// A single isolated file.
pub(crate) struct IsolatedFile {
  pub(crate) artifacts: FileArtifacts,
  pub(crate) errors: FileErrors,
  pub(crate) eval: JsonnetFile,
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
    let mut tys = jsonnet_statics::ty::Store::default();
    let st = jsonnet_statics::st::St::new(&mut tys);
    let statics = jsonnet_statics::get(st, &desugar.arenas, desugar.top);
    let combine = jsonnet_expr::Artifacts { paths: desugar.ps, strings: desugar.arenas.str };
    let mut ret = Self {
      artifacts: FileArtifacts {
        pos_db: text_pos::PositionDb::new(contents),
        syntax: parse.root,
        pointers: desugar.pointers,
        defs: statics.defs,
        tys,
        expr_tys: statics.expr_tys,
      },
      errors: FileErrors {
        lex: lex.errors,
        parse: parse.errors,
        desugar: desugar.errors,
        statics: statics.errors,
      },
      eval: JsonnetFile { expr_ar: desugar.arenas.expr, top: desugar.top },
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
    ret.artifacts.tys.apply(&subst);
    ret
  }

  pub(crate) fn from_fs<F>(
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

  pub(crate) fn from_str<F>(
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

  pub(crate) fn diagnostics<'a>(
    &'a self,
    strings: &'a jsonnet_expr::StrArena,
    root: &'a jsonnet_syntax::kind::SyntaxNode,
  ) -> impl Iterator<Item = Diagnostic> + 'a {
    let all_errors = std::iter::empty()
      .chain(self.errors.lex.iter().map(|err| (err.range(), err.to_string())))
      .chain(self.errors.parse.iter().map(|err| (err.range(), err.to_string())))
      .chain(self.errors.desugar.iter().map(|err| (err.range(), err.to_string())))
      .map(|(r, m)| (r, m, diagnostic::Severity::Error));
    all_errors
      .chain(self.errors.statics.iter().map(|err| {
        let (expr, kind) = err.expr_and_def();
        let range = expr_range(&self.artifacts.pointers, root, expr, kind);
        let msg = err.display(strings);
        (range, msg.to_string(), err.severity())
      }))
      .filter_map(|(range, message, severity)| {
        let Some(range) = self.artifacts.pos_db.range_utf16(range) else {
          always!(false, "bad range: {range:?}");
          return None;
        };
        Some(Diagnostic { range, message, severity })
      })
  }
}

/// An I/O error with an associated path.
#[derive(Debug)]
pub struct PathIoError {
  pub(crate) path: std::path::PathBuf,
  pub(crate) error: std::io::Error,
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

/// A [`std::result::Result`] with the error type defaulting to [`PathIoError`].
pub(crate) type Result<T, E = PathIoError> = std::result::Result<T, E>;

pub(crate) fn expr_range(
  pointers: &jsonnet_desugar::Pointers,
  root: &jsonnet_syntax::kind::SyntaxNode,
  expr: jsonnet_expr::ExprMust,
  kind: Option<def::ExprDefKind>,
) -> text_size::TextRange {
  let maybe_more_precise = kind.and_then(|kind| expr_def_range(pointers, root, expr, kind));
  maybe_more_precise.unwrap_or_else(|| pointers.get_ptr(expr).text_range())
}

fn expr_def_range(
  pointers: &jsonnet_desugar::Pointers,
  root: &jsonnet_syntax::kind::SyntaxNode,
  expr: jsonnet_expr::ExprMust,
  kind: def::ExprDefKind,
) -> Option<text_size::TextRange> {
  let node_ptr = pointers.get_ptr(expr);
  match kind {
    def::ExprDefKind::ObjectCompId => {
      let obj = node_ptr.cast::<jsonnet_syntax::ast::Object>()?;
      let obj = obj.try_to_node(root)?;
      let comp_spec = obj.comp_specs().next()?;
      match comp_spec {
        jsonnet_syntax::ast::CompSpec::ForSpec(for_spec) => Some(for_spec.id()?.text_range()),
        jsonnet_syntax::ast::CompSpec::IfSpec(_) => None,
      }
    }
    // NOTE because of desugaring, not all expr locals are actually from ast locals. we try to
    // get the exact location first and then fall back.
    def::ExprDefKind::LocalBind(idx) => node_ptr
      .cast::<jsonnet_syntax::ast::ExprLocal>()
      .and_then(|local| {
        let local = local.try_to_node(root)?;
        Some(local.bind_commas().nth(idx)?.bind()?.id()?.text_range())
      })
      .or_else(|| {
        log::warn!("local fallback: {node_ptr:?}");
        let node = node_ptr.try_to_node(root)?;
        log::warn!("node: {node:?}");
        Some(node.text_range())
      }),
    // NOTE because of desugaring, possibly not all expr fns are actually from ast fns
    def::ExprDefKind::FnParam(idx) => node_ptr
      .cast::<jsonnet_syntax::ast::ExprFunction>()
      .and_then(|func| {
        let func = func.try_to_node(root)?;
        Some(func.paren_params()?.params().nth(idx)?.id()?.text_range())
      })
      .or_else(|| {
        log::warn!("func fallback: {node_ptr:?}");
        let node = node_ptr.try_to_node(root)?;
        log::warn!("node: {node:?}");
        Some(node.text_range())
      }),
    def::ExprDefKind::ObjectLocal(idx) => node_ptr
      .cast::<jsonnet_syntax::ast::ExprObject>()
      .and_then(|obj| {
        let obj = obj.try_to_node(root)?.object()?;
        let nth_local = obj
          .members()
          .filter_map(|x| match x.member_kind()? {
            jsonnet_syntax::ast::MemberKind::ObjectLocal(loc) => Some(loc),
            _ => None,
          })
          .nth(idx)?;
        Some(nth_local.bind()?.id()?.text_range())
      })
      .or_else(|| {
        log::warn!("object local fallback: {node_ptr:?}");
        let node = node_ptr.try_to_node(root)?;
        log::warn!("node: {node:?}");
        Some(node.text_range())
      }),
  }
}
