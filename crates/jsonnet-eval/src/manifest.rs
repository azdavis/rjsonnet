//! Manifesting Jsonnet values into JSON values.

use crate::val::{json, jsonnet};
use crate::{error, exec, Cx};
use always::always;
use std::collections::BTreeMap;

/// Manifests the Jsonnet value into a JSON value.
///
/// # Errors
///
/// If manifestation failed.
pub fn get(cx: Cx<'_>, val: jsonnet::Val) -> error::Result<json::Val> {
  match val {
    jsonnet::Val::Prim(prim) => Ok(json::Val::Prim(prim)),
    jsonnet::Val::Object(object) => {
      for (env, expr) in object.asserts() {
        get_(cx, &env, expr)?;
      }
      let mut val_fields = BTreeMap::<jsonnet_expr::Str, json::Val>::default();
      for (name, vis, field) in object.fields() {
        if matches!(vis, jsonnet_expr::Visibility::Hidden) {
          continue;
        }
        let jsonnet::Field::Expr(env, expr) = field else {
          always!(false, "non-hidden std field");
          continue;
        };
        let val = get_(cx, &env, expr)?;
        always!(val_fields.insert(name, val).is_none());
      }
      Ok(json::Val::Object(val_fields))
    }
    jsonnet::Val::Array(parts) => {
      let iter = parts.iter().map(|(env, elem)| get_(cx, env, elem));
      let vs = iter.collect::<error::Result<Vec<_>>>()?;
      Ok(json::Val::Array(vs))
    }
    jsonnet::Val::Function { .. } | jsonnet::Val::StdFn(_) => Err(error::Error::ManifestFn),
  }
}

/// both executes and manifests
fn get_(cx: Cx<'_>, env: &jsonnet::Env, expr: jsonnet_expr::Expr) -> error::Result<json::Val> {
  let val = exec::get(cx, env, expr)?;
  get(cx, val)
}
