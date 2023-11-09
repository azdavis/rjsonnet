//! Manifesting Jsonnet values into JSON values.

use crate::val::{json, jsonnet};
use crate::{error, exec};
use jsonnet_expr::Arenas;
use rustc_hash::FxHashMap;

/// Manifests the Jsonnet value into a JSON value.
///
/// # Errors
///
/// If manifestation failed.
///
/// # Panics
///
/// Upon internal error.
pub fn get(ars: &Arenas, val: jsonnet::Val) -> error::Result<json::Val> {
  match val {
    jsonnet::Val::Prim(prim) => Ok(json::Val::Prim(prim)),
    jsonnet::Val::Rec { env, kind } => match kind {
      jsonnet::RecValKind::Object { asserts, fields } => {
        for expr in asserts {
          get_(&env, ars, expr)?;
        }
        let mut val_fields = FxHashMap::default();
        for (name, (vis, expr)) in fields {
          if matches!(vis, jsonnet_expr::Visibility::Hidden) {
            continue;
          }
          let val = get_(&env, ars, expr)?;
          assert!(val_fields.insert(name, val).is_none());
        }
        Ok(json::Val::Object(val_fields))
      }
      jsonnet::RecValKind::Function { .. } => Err(error::Error::ManifestFn),
      jsonnet::RecValKind::Array(exprs) => {
        let iter = exprs.into_iter().map(|expr| get_(&env, ars, expr));
        let vs = iter.collect::<error::Result<Vec<_>>>()?;
        Ok(json::Val::Array(vs))
      }
    },
    jsonnet::Val::Std(std_val) => match std_val {
      jsonnet::Std::Cmp | jsonnet::Std::Equals => Err(error::Error::ManifestFn),
    },
  }
}

/// both executes and manifests
fn get_(env: &jsonnet::Env, ars: &Arenas, expr: jsonnet_expr::Expr) -> error::Result<json::Val> {
  let val = exec::get(env, ars, expr)?;
  get(ars, val)
}
