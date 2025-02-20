//! Manifesting Jsonnet values into JSON values.

use crate::{error, exec, Cx};
use always::always;
use jsonnet_val::{json, jsonnet};
use std::collections::BTreeMap;

/// Manifests the Jsonnet value into a JSON value.
///
/// # Errors
///
/// If manifestation failed.
pub fn get(cx: &mut Cx<'_>, val: jsonnet::Val) -> error::Result<json::Val> {
  match val {
    jsonnet::Val::Prim(prim) => Ok(json::Val::Prim(prim)),
    jsonnet::Val::Object(object) => {
      exec::ck_object_asserts(cx, &object)?;
      let mut val_fields = BTreeMap::<jsonnet_expr::Str, json::Val>::default();
      for (name, field) in object.fields() {
        let (env, expr) = match field {
          jsonnet::Field::Expr(vis, env, expr) => {
            if matches!(vis, jsonnet_expr::Vis::Hidden) {
              continue;
            }
            (env, expr)
          }
          // always hidden
          jsonnet::Field::Std(_) => continue,
        };
        let val = get_(cx, &env, expr)?;
        always!(val_fields.insert(name, val).is_none());
      }
      Ok(json::Val::Object(val_fields))
    }
    jsonnet::Val::Array(parts) => {
      let iter = parts.elems().into_iter().map(|(env, expr)| get_(cx, env, expr));
      let vs = iter.collect::<error::Result<Vec<_>>>()?;
      Ok(json::Val::Array(vs))
    }
    jsonnet::Val::Fn(_) => Err(error::Error::ManifestFn),
  }
}

/// both executes and manifests
fn get_(cx: &mut Cx<'_>, env: &jsonnet::Env, expr: jsonnet_expr::Expr) -> error::Result<json::Val> {
  let val = exec::get(cx, env, expr)?;
  get(cx, val)
}
