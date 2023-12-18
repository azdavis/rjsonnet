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
pub fn get(ars: &Arenas, this: &jsonnet::Object, val: jsonnet::Val) -> error::Result<json::Val> {
  match val {
    jsonnet::Val::Prim(prim) => Ok(json::Val::Prim(prim)),
    jsonnet::Val::Object(object) => {
      for (env, expr) in object.asserts() {
        let cx = exec::Cx::new(env, &object);
        get_(cx, ars, expr)?;
      }
      let mut val_fields = FxHashMap::default();
      for (env, name, vis, expr) in object.fields() {
        if matches!(vis, jsonnet_expr::Visibility::Hidden) {
          continue;
        }
        let cx = exec::Cx::new(env, &object);
        let val = get_(cx, ars, expr)?;
        assert!(val_fields.insert(name.clone(), val).is_none());
      }
      Ok(json::Val::Object(val_fields))
    }
    jsonnet::Val::Array(parts) => {
      let iter = parts.iter().map(|(env, elem)| {
        let cx = exec::Cx::new(env, this);
        get_(cx, ars, elem)
      });
      let vs = iter.collect::<error::Result<Vec<_>>>()?;
      Ok(json::Val::Array(vs))
    }
    jsonnet::Val::Function { .. } | jsonnet::Val::StdFn(_) => Err(error::Error::ManifestFn),
  }
}

/// both executes and manifests
fn get_(cx: exec::Cx<'_>, ars: &Arenas, expr: jsonnet_expr::Expr) -> error::Result<json::Val> {
  let val = exec::get(cx, ars, expr)?;
  get(ars, cx.this(), val)
}
