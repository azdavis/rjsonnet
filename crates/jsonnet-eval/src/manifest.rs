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
    jsonnet::Val::Object { mut env, asserts, fields } => {
      exec::insert_obj_self_super(&mut env, asserts.clone(), fields.clone());
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
    jsonnet::Val::Function { .. } => Err(error::Error::ManifestFn),
    jsonnet::Val::Array(parts) => {
      let iter = parts
        .into_iter()
        .flat_map(|part| part.elems.into_iter().map(move |elem| get_(&part.env, ars, elem)));
      let vs = iter.collect::<error::Result<Vec<_>>>()?;
      Ok(json::Val::Array(vs))
    }
    jsonnet::Val::StdFn(std_val) => match std_val {
      jsonnet::StdFn::Cmp | jsonnet::StdFn::Equals => Err(error::Error::ManifestFn),
    },
  }
}

/// both executes and manifests
fn get_(env: &jsonnet::Env, ars: &Arenas, expr: jsonnet_expr::Expr) -> error::Result<json::Val> {
  let val = exec::get(env, ars, expr)?;
  get(ars, val)
}
