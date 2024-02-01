//! Analyze jsonnet files.

use paths::PathId;
use rayon::iter::{IndexedParallelIterator as _, IntoParallelIterator as _, ParallelIterator as _};
use std::path::{Path, PathBuf};

/// The state of analysis.
#[derive(Debug)]
pub struct St<F> {
  fs: F,
  artifacts: jsonnet_expr::Artifacts,
  files: paths::PathMap<jsonnet_eval::JsonnetFile>,
}

impl<F> St<F>
where
  F: Sync + Send + paths::FileSystem,
{
  /// Returns a new state.
  pub fn new(fs: F) -> Self {
    Self { fs, artifacts: jsonnet_expr::Artifacts::default(), files: paths::PathMap::default() }
  }

  /// Updates the state with added and removed Jsonnet paths.
  ///
  /// # Panics
  ///
  /// If the paths couldn't be read, or weren't file paths, or couldn't canonicalize, or if they had
  /// any jsonnet errors.
  pub fn update_many(&mut self, remove: Vec<PathBuf>, add: Vec<PathBuf>) {
    for path in remove {
      let path_id = self.path_id(path.as_path());
      self.files.remove(&path_id);
    }

    let par_iter = add.into_par_iter().map(|path| {
      let contents = self.fs.read_to_string(path.as_path()).expect("read to string");
      let parent = path.parent().expect("no parent");
      let artifacts = FileArtifacts::new(contents.as_str(), parent, &FsAdapter(&self.fs));
      (path, artifacts)
    });
    let mut file_artifacts = Vec::<(PathBuf, FileArtifacts)>::default();
    par_iter.collect_into_vec(&mut file_artifacts);

    for (path, artifacts) in file_artifacts {
      artifacts.panic_if_any_errors();
      let mut file = jsonnet_eval::JsonnetFile {
        expr_ar: artifacts.desugar.arenas.expr,
        top: artifacts.desugar.top,
      };
      let artifacts = jsonnet_expr::Artifacts {
        paths: artifacts.desugar.ps,
        strings: artifacts.desugar.arenas.str,
      };
      jsonnet_expr::combine::get(&mut self.artifacts, artifacts, &mut file.expr_ar);
      let path_id = self.path_id(path.as_path());
      self.files.insert(path_id, file);
    }
  }

  /// Return an evaluation context from this.
  pub fn cx(&self) -> jsonnet_eval::Cx<'_> {
    jsonnet_eval::Cx {
      jsonnet_files: &self.files,
      paths: &self.artifacts.paths,
      str_ar: &self.artifacts.strings,
    }
  }

  /// Returns a path id for this path.
  ///
  /// # Panics
  ///
  /// If we could not canonicalize the path.
  pub fn path_id(&mut self, path: &Path) -> PathId {
    let canonical = self.fs.canonicalize(path).expect("canonicalize");
    self.artifacts.paths.get_id(&canonical)
  }

  /// Returns the string arena for this.
  pub fn strings(&self) -> &jsonnet_expr::StrArena {
    &self.artifacts.strings
  }

  /// Returns the mutable string arena for this.
  pub fn strings_mut(&mut self) -> &mut jsonnet_expr::StrArena {
    &mut self.artifacts.strings
  }
}

/// Artifacts from a file analyzed in isolation.
#[derive(Debug)]
struct FileArtifacts {
  lex_errors: Vec<jsonnet_lex::Error>,
  parse: jsonnet_parse::Parse,
  desugar: jsonnet_desugar::Desugar,
  statics_errors: Vec<jsonnet_statics::error::Error>,
}

impl FileArtifacts {
  /// Returns artifacts for a file contained in the given directory.
  fn new(contents: &str, current_dir: &Path, fs: &dyn jsonnet_desugar::FileSystem) -> Self {
    let lex = jsonnet_lex::get(contents);
    let parse = jsonnet_parse::get(&lex.tokens);
    let desugar = jsonnet_desugar::get(current_dir, &[], fs, parse.root.clone().into_ast());
    let statics_errors = jsonnet_statics::get(&desugar.arenas, desugar.top);
    Self { lex_errors: lex.errors, parse, desugar, statics_errors }
  }

  fn panic_if_any_errors(&self) {
    if let Some(e) = self.lex_errors.first() {
      panic!("lex error: {e}");
    }
    if let Some(e) = self.parse.errors.first() {
      panic!("parse error: {e}");
    }
    if let Some(e) = self.desugar.errors.first() {
      panic!("desugar error: {e}");
    }
    if let Some(e) = self.statics_errors.first() {
      let e = e.display(&self.desugar.arenas.str);
      panic!("statics error: {e}");
    }
  }
}

/// An adaptor between file system traits.
struct FsAdapter<'a, F>(&'a F);

impl<'a, F> jsonnet_desugar::FileSystem for FsAdapter<'a, F>
where
  F: paths::FileSystem,
{
  fn canonicalize(&self, p: &Path) -> std::io::Result<paths::CanonicalPathBuf> {
    self.0.canonicalize(p)
  }
}
