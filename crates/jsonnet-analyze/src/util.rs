//! Utilities.

use always::always;
use diagnostic::Diagnostic;
use jsonnet_expr::def;
use jsonnet_syntax::{ast::AstNode as _, kind::SyntaxKind as SK};
use std::fmt;
use token::Triviable as _;

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
  /// Whether to display types on multiple lines.
  pub multi_line: jsonnet_ty::display::MultiLine,
}

/// An adaptor between file system traits.
pub(crate) struct FsAdapter<'a, F>(pub(crate) &'a F);

impl<F> jsonnet_resolve_import::FileSystem for FsAdapter<'_, F>
where
  F: paths::FileSystem,
{
  fn is_file(&self, p: &std::path::Path) -> bool {
    paths::FileSystem::is_file(self.0, p)
  }
}

/// Things shared across all files.
#[derive(Debug, Default)]
pub(crate) struct GlobalArtifacts {
  pub(crate) syntax: jsonnet_expr::Artifacts,
  pub(crate) statics: jsonnet_ty::GlobalStore,
}

#[derive(Debug)]
pub(crate) struct SyntaxFileArtifacts {
  pub(crate) pos_db: text_pos::PositionDb,
  pub(crate) root: jsonnet_syntax::Root,
  pub(crate) pointers: jsonnet_desugar::Pointers,
}

#[derive(Debug, Default)]
pub(crate) struct SyntaxFileErrors {
  pub(crate) lex: Vec<jsonnet_lex::Error>,
  pub(crate) parse: Vec<jsonnet_parse::Error>,
  pub(crate) desugar: Vec<jsonnet_desugar::Error>,
}

impl SyntaxFileErrors {
  fn is_empty(&self) -> bool {
    self.lex.is_empty() && self.parse.is_empty() && self.desugar.is_empty()
  }
}

#[derive(Debug)]
pub(crate) struct SyntaxFile {
  pub(crate) artifacts: SyntaxFileArtifacts,
  pub(crate) errors: SyntaxFileErrors,
  pub(crate) exprs: jsonnet_eval::Exprs,
}

/// ALL fields MUST be private.
///
/// It used to be that we combined syntax and statics in one step, so there was no need for separate
/// types. But that doesn't work because we need the path ids to be combined by the time we're
/// looking at inter-file import types, as we wish to in statics.
#[derive(Debug)]
pub(crate) struct SyntaxFileToCombine {
  file: SyntaxFile,
  to_combine: jsonnet_expr::Artifacts,
}

impl SyntaxFileToCombine {
  fn new(
    contents: &str,
    dirs: jsonnet_resolve_import::NonEmptyDirs<'_>,
    fs: &dyn jsonnet_resolve_import::FileSystem,
  ) -> Self {
    let lex = jsonnet_lex::get(contents);
    let parse = jsonnet_parse::get(&lex.tokens);
    let root = parse.root.clone().into_ast().and_then(|x| x.expr());
    let desugar = jsonnet_desugar::get(dirs, fs, root);
    Self {
      file: SyntaxFile {
        artifacts: SyntaxFileArtifacts {
          pos_db: text_pos::PositionDb::new(contents),
          root: parse.root,
          pointers: desugar.pointers,
        },
        errors: SyntaxFileErrors { lex: lex.errors, parse: parse.errors, desugar: desugar.errors },
        exprs: jsonnet_eval::Exprs { ar: desugar.arenas.expr, top: desugar.top },
      },
      to_combine: jsonnet_expr::Artifacts { paths: desugar.ps, strings: desugar.arenas.str },
    }
  }

  pub(crate) fn from_str<F>(
    path: &paths::CleanPath,
    contents: &str,
    root_dirs: &[paths::CleanPathBuf],
    fs: &F,
  ) -> Self
  where
    F: paths::FileSystem,
  {
    let parent = path_parent_must(path);
    let dirs = jsonnet_resolve_import::NonEmptyDirs::new(parent, root_dirs);
    Self::new(contents, dirs, &FsAdapter(fs))
  }

  pub(crate) fn from_fs<F>(
    path: &paths::CleanPath,
    root_dirs: &[paths::CleanPathBuf],
    fs: &F,
  ) -> std::io::Result<Self>
  where
    F: paths::FileSystem,
  {
    let contents = fs.read_to_string(path.as_path())?;
    Ok(Self::from_str(path, contents.as_str(), root_dirs, fs))
  }

  pub(crate) fn combine(self, artifacts: &mut GlobalArtifacts) -> SyntaxFile {
    let mut ret = self.file;
    let expr_subst = jsonnet_expr::Subst::get(&mut artifacts.syntax, self.to_combine);
    for (_, ed) in ret.exprs.ar.iter_mut() {
      ed.apply(&expr_subst);
    }
    ret
  }
}

#[derive(Debug)]
pub(crate) struct StaticsFile {
  pub(crate) syntax: SyntaxFile,
  pub(crate) statics: jsonnet_statics::st::Statics,
}

impl StaticsFile {
  /// returns whether this has NO errors.
  pub(crate) fn is_clean(&self) -> bool {
    self.syntax.errors.is_empty()
      && self.statics.errors.iter().all(|x| matches!(x.severity(), diagnostic::Severity::Warning))
  }

  pub(crate) fn diagnostics<'a>(
    &'a self,
    multi_line: jsonnet_ty::display::MultiLine,
    store: &'a jsonnet_ty::GlobalStore,
    str_ar: &'a jsonnet_expr::StrArena,
  ) -> impl Iterator<Item = Diagnostic> + 'a {
    let root = self.syntax.artifacts.root.clone();
    let root = root.syntax();
    let all_errors = std::iter::empty()
      .chain(self.syntax.errors.lex.iter().map(|err| (err.range(), err.to_string())))
      .chain(self.syntax.errors.parse.iter().map(|err| (err.range(), err.to_string())))
      .chain(self.syntax.errors.desugar.iter().map(|err| (err.range(), err.to_string())))
      .map(|(r, m)| (r, m, diagnostic::Severity::Error));
    all_errors
      .chain(self.statics.errors.iter().map(move |err| {
        let (expr, kind) = err.expr_and_def();
        let range = expr_range(&self.syntax.artifacts.pointers, &root, expr, kind);
        let msg = err.display(multi_line, store, str_ar);
        (range, msg.to_string(), err.severity())
      }))
      .filter_map(|(range, message, severity)| {
        let Some(range) = self.syntax.artifacts.pos_db.range_utf16(range) else {
          always!(false, "bad range: {range:?}");
          return None;
        };
        Some(Diagnostic { range, message, severity })
      })
  }

  pub(crate) fn into_artifacts(self) -> FileArtifacts {
    FileArtifacts {
      syntax: self.syntax.artifacts,
      defs: self.statics.defs,
      expr_tys: self.statics.expr_tys,
    }
  }
}

/// ALL fields MUST be private.
#[derive(Debug)]
pub(crate) struct StaticsFileToCombine {
  file: StaticsFile,
  to_combine: jsonnet_ty::LocalStore,
}

impl StaticsFileToCombine {
  pub(crate) fn new(
    syntax: SyntaxFile,
    artifacts: &GlobalArtifacts,
    file_tys: &paths::PathMap<jsonnet_ty::Ty>,
  ) -> Self {
    let st = jsonnet_statics::st::St::new(&artifacts.statics, file_tys, &artifacts.syntax.strings);
    let (statics, to_combine) = jsonnet_statics::get(st, &syntax.exprs.ar, syntax.exprs.top);
    Self { file: StaticsFile { syntax, statics }, to_combine }
  }

  pub(crate) fn combine(self, artifacts: &mut GlobalArtifacts) -> StaticsFile {
    let mut ret = self.file;
    let ty_subst = jsonnet_ty::Subst::get(&mut artifacts.statics, self.to_combine);
    for err in &mut ret.statics.errors {
      err.apply(&ty_subst);
    }
    for ty in ret.statics.expr_tys.values_mut() {
      ty.apply(&ty_subst);
    }
    ret
  }
}

#[derive(Debug)]
pub(crate) struct FileArtifacts {
  pub(crate) syntax: SyntaxFileArtifacts,
  pub(crate) defs: jsonnet_expr::def::Map,
  pub(crate) expr_tys: jsonnet_ty::Exprs,
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
  maybe_more_precise.or_else(|| Some(pointers.get_ptr(expr)?.text_range())).unwrap_or_default()
}

fn expr_def_range(
  pointers: &jsonnet_desugar::Pointers,
  root: &jsonnet_syntax::kind::SyntaxNode,
  expr: jsonnet_expr::ExprMust,
  kind: def::ExprDefKind,
) -> Option<text_size::TextRange> {
  let node_ptr = pointers.get_ptr(expr)?;
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
    def::ExprDefKind::Multi(idx, def::ExprDefKindMulti::LocalBind) => node_ptr
      .cast::<jsonnet_syntax::ast::ExprLocal>()
      .and_then(|local| {
        let local = local.try_to_node(root)?;
        Some(local.bind_commas().nth(idx)?.bind()?.id()?.text_range())
      })
      .or_else(|| {
        // this helps with `[... for x in xs]` desugaring
        let for_spec = node_ptr.cast::<jsonnet_syntax::ast::ForSpec>()?;
        let for_spec = for_spec.try_to_node(root)?;
        Some(for_spec.id()?.text_range())
      })
      .or_else(|| {
        let paren_params = node_ptr.cast::<jsonnet_syntax::ast::ParenParams>()?;
        let paren_params = paren_params.try_to_node(root)?;
        let parent = paren_params.syntax().parent()?;
        // this helps with `local f(x) = ...` desugaring
        if let Some(bind) = jsonnet_syntax::ast::Bind::cast(parent.clone()) {
          return Some(bind.expr()?.syntax().text_range());
        }
        // this helps with `{ f(x): ... }` desugaring
        if let Some(field) = jsonnet_syntax::ast::Field::cast(parent) {
          return Some(field.expr()?.syntax().text_range());
        }
        None
      })
      .or_else(|| {
        log::warn!("local fallback: {node_ptr:?}");
        let node = node_ptr.try_to_node(root)?;
        log::warn!("node: {node:?}");
        Some(node.text_range())
      }),
    // NOTE because of desugaring, possibly not all expr fns are actually from ast fns
    def::ExprDefKind::Multi(idx, def::ExprDefKindMulti::FnParam) => node_ptr
      .cast::<jsonnet_syntax::ast::ExprFunction>()
      .and_then(|func| {
        let func = func.try_to_node(root)?;
        Some(func.paren_params()?.params().nth(idx)?.id()?.text_range())
      })
      .or_else(|| {
        // this helps with `local f(x) = ...` desugaring
        let paren_params = node_ptr.cast::<jsonnet_syntax::ast::ParenParams>()?;
        let paren_params = paren_params.try_to_node(root)?;
        Some(paren_params.params().nth(idx)?.id()?.text_range())
      })
      .or_else(|| {
        log::warn!("func fallback: {node_ptr:?}");
        let node = node_ptr.try_to_node(root)?;
        log::warn!("node: {node:?}");
        Some(node.text_range())
      }),
    def::ExprDefKind::Multi(idx, def::ExprDefKindMulti::ObjectLocalBind) => node_ptr
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

pub(crate) fn approximate_code_imports(contents: &str) -> Vec<String> {
  let mut next = false;
  let mut ret = Vec::<String>::new();
  for tok in jsonnet_lex::get(contents).tokens {
    if tok.kind.is_trivia() {
      continue;
    }
    if tok.kind == SK::ImportKw {
      next = true;
      continue;
    }
    if !next {
      continue;
    }
    next = false;
    let s = match tok.kind {
      SK::DoubleQuotedString => jsonnet_ast_escape::double_quoted(tok.text),
      SK::DoubleQuotedVerbatimString => jsonnet_ast_escape::double_quoted_verbatim(tok.text),
      SK::SingleQuotedString => jsonnet_ast_escape::single_quoted(tok.text),
      SK::SingleQuotedVerbatimString => jsonnet_ast_escape::single_quoted_verbatim(tok.text),
      _ => continue,
    };
    ret.push(s);
  }
  ret
}

pub(crate) fn path_parent_must(path: &paths::CleanPath) -> &paths::CleanPath {
  if let Some(x) = path.parent() {
    x
  } else {
    always!(false, "no parent for {}", path.as_path().display());
    path
  }
}
