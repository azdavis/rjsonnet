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
mod val;

#[derive(Debug, Clone, Copy)]
pub struct Cx<'a> {
  pub paths: &'a paths::Store,
  pub jsonnet_files: &'a paths::PathMap<JsonnetFile>,
  pub str_ar: &'a jsonnet_expr::StrArena,
}

#[derive(Debug)]
pub struct JsonnetFile {
  pub expr_ar: jsonnet_expr::ExprArena,
  pub top: jsonnet_expr::Expr,
}

#[derive(Debug)]
pub struct Jsonnet(val::jsonnet::Val);

/// Executes the Jsonnet expression to produce a Jsonnet value.
///
/// # Errors
///
/// If execution failed.
///
/// # Panics
///
/// If the expr wasn't checked.
pub fn get_exec(cx: Cx<'_>, path: paths::PathId) -> error::Result<Jsonnet> {
  let file = cx.jsonnet_files.get(&path).expect("no path");
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
    self.0.display(ar)
  }

  /// Asserts this is a string.
  ///
  /// # Panics
  ///
  /// If it wasn't.
  pub fn assert_is_str(&self, ar: &jsonnet_expr::StrArena, want: &jsonnet_expr::Str) {
    let val::json::Val::Prim(jsonnet_expr::Prim::String(got)) = &self.0 else {
      panic!("did not get a String")
    };
    if want != got {
      let want = ar.get(want);
      let got = ar.get(got);
      panic!("want: {want:?}\ngot:  {got:?}")
    }
  }
}

/// Manifests the Jsonnet value into a JSON value.
///
/// # Errors
///
/// If manifestation failed.
///
/// # Panics
///
/// Upon internal error.
pub fn get_manifest(cx: Cx<'_>, val: Jsonnet) -> error::Result<Json> {
  manifest::get(cx, val.0).map(Json)
}
