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

#![expect(missing_docs, clippy::too_many_lines)]

mod generated {
  include!(concat!(env!("OUT_DIR"), "/generated.rs"));
}

pub mod error;

mod exec;
mod manifest;
mod std_lib;
mod util;

#[derive(Debug)]
pub struct Cx<'a> {
  pub paths: &'a paths::Store,
  pub str_ar: &'a mut jsonnet_expr::StrArena,
  pub exprs: &'a mut paths::PathMap<Exprs>,
  pub import_str: &'a paths::PathMap<String>,
  pub import_bin: &'a paths::PathMap<Vec<u8>>,
  pub obj_mk: jsonnet_val::jsonnet::ObjectMk,
}

#[derive(Debug)]
pub struct Exprs {
  pub ar: jsonnet_expr::ExprArena,
  pub top: jsonnet_expr::Expr,
}

#[derive(Debug, Clone, Copy)]
pub struct Import {
  pub expr: jsonnet_expr::ExprMust,
  pub kind: jsonnet_expr::ImportKind,
  pub path: paths::PathId,
}

impl Exprs {
  pub fn imports(&self) -> impl Iterator<Item = Import> {
    self.ar.iter().filter_map(|(expr, ed)| {
      if let jsonnet_expr::ExprData::Import { kind, path } = *ed {
        Some(Import { expr, kind, path })
      } else {
        None
      }
    })
  }
}

/// Executes the Jsonnet expression to produce a Jsonnet value.
///
/// # Errors
///
/// If execution failed.
pub fn get_exec(cx: &mut Cx<'_>, path: paths::PathId) -> error::Result<jsonnet_val::jsonnet::Val> {
  let Some(file) = cx.exprs.get(&path) else { return Err(error::Error::NoPath(path)) };
  let env = jsonnet_val::jsonnet::Env::empty(path);
  exec::get(cx, &env, file.top)
}

/// Manifests the Jsonnet value into a JSON value.
///
/// # Errors
///
/// If manifestation failed.
pub fn get_manifest(
  cx: &mut Cx<'_>,
  val: jsonnet_val::jsonnet::Val,
) -> error::Result<jsonnet_val::json::Val> {
  manifest::get(cx, val)
}

pub(crate) fn mk_todo(expr: jsonnet_expr::ExprMust, msg: &'static str) -> error::Error {
  error::Error::Exec { expr, kind: error::Kind::Todo(msg) }
}
