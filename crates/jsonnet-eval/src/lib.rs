//! The dynamic semantics of Jsonnet.
//!
//! There are two main operations:
//!
//! - Execution: from Jsonnet expressions to Jsonnet values.
//! - Manifestation: from Jsonnet values to JSON values.
//!
//! These are both mutually recursive:
//!
//! - Jsonnet values are lazy. They can thus contain unexecuted Jsonnet expressions, which must be
//!   executed to produce values.
//! - During execution, we may need to manifest a Jsonnet value to convert it to a string.

#![allow(missing_docs, clippy::too_many_lines)]

pub mod error;

mod exec;
mod manifest;
mod std_lib;
mod val;

#[derive(Debug, Clone, Copy)]
pub struct Cx<'a> {
  pub paths: &'a paths::Store,
  pub jsonnet_files: &'a paths::PathMap<JsonnetFile>,
  pub importstr: &'a paths::PathMap<String>,
  pub importbin: &'a paths::PathMap<Vec<u8>>,
  pub str_ar: &'a jsonnet_expr::StrArena,
}

#[derive(Debug)]
pub struct JsonnetFile {
  pub expr_ar: jsonnet_expr::ExprArena,
  pub top: jsonnet_expr::Expr,
}

#[derive(Debug, Clone, Copy)]
pub struct Import {
  pub expr: jsonnet_expr::ExprMust,
  pub kind: jsonnet_expr::ImportKind,
  pub path: paths::PathId,
}

impl JsonnetFile {
  pub fn imports(&self) -> impl Iterator<Item = Import> + '_ {
    self.expr_ar.iter().filter_map(|(expr, ed)| {
      if let jsonnet_expr::ExprData::Import { kind, path } = *ed {
        Some(Import { expr, kind, path })
      } else {
        None
      }
    })
  }
}

#[derive(Debug)]
pub struct Jsonnet(val::jsonnet::Val);

/// Executes the Jsonnet expression to produce a Jsonnet value.
///
/// # Errors
///
/// If execution failed.
pub fn get_exec(cx: Cx<'_>, path: paths::PathId) -> error::Result<Jsonnet> {
  let Some(file) = cx.jsonnet_files.get(&path) else { return Err(error::Error::NoPath(path)) };
  let env = val::jsonnet::Env::new(path);
  exec::get(cx, &env, file.top).map(Jsonnet)
}

#[derive(Debug, PartialEq, Eq)]
pub struct Json(val::json::Val);

impl Json {
  /// Convert from serde.
  #[must_use]
  pub fn from_serde(ar: &jsonnet_expr::StrArena, serde: serde_json::Value) -> Self {
    Json(val::json::Val::from_serde(ar, serde))
  }

  #[must_use]
  pub fn display<'a>(&'a self, ar: &'a jsonnet_expr::StrArena) -> impl std::fmt::Display + 'a {
    self.0.display(ar, 0)
  }

  /// Asserts this is a string.
  ///
  /// # Panics
  ///
  /// If it wasn't.
  #[cfg(feature = "testing")]
  pub fn assert_is_str(&self, ar: &jsonnet_expr::StrArena, want: &str) {
    let val::json::Val::Prim(jsonnet_expr::Prim::String(got)) = &self.0 else {
      panic!("did not get a String")
    };
    let got = ar.get(got);
    assert_eq!(want, got);
  }
}

/// Manifests the Jsonnet value into a JSON value.
///
/// # Errors
///
/// If manifestation failed.
pub fn get_manifest(cx: Cx<'_>, val: Jsonnet) -> error::Result<Json> {
  manifest::get(cx, val.0).map(Json)
}

pub(crate) fn mk_todo(expr: jsonnet_expr::ExprMust, msg: &'static str) -> error::Error {
  error::Error::Exec { expr, kind: error::Kind::Todo(msg) }
}
