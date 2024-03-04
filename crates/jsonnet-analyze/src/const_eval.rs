//! Try to "evaluate" without actually evaluating, using statically-known information.

use crate::St;
use jsonnet_expr::{Expr, ExprData, ExprMust, Prim};
use jsonnet_statics::Def;
use paths::PathId;

#[derive(Debug)]
pub(crate) struct ExprWithPath {
  pub(crate) path_id: PathId,
  pub(crate) expr: ExprMust,
}

impl ExprWithPath {
  fn expr_data<'s>(&self, st: &'s St) -> &'s ExprData {
    &st.files[&self.path_id].expr_ar[self.expr]
  }
}

#[derive(Debug)]
pub(crate) struct ConstEval {
  pub(crate) ewp: ExprWithPath,
  pub(crate) kind: Kind,
}

#[derive(Debug)]
pub(crate) enum Kind {
  Expr,
  ObjectCompId,
  LocalBind(usize),
  FunctionParam(usize),
}

pub(crate) fn get(st: &St, path_id: PathId, expr: Expr) -> Option<ConstEval> {
  let ewp = ExprWithPath { path_id, expr: expr? };
  if let Some(&def) = st.file_artifacts[&path_id].defs.get(&ewp.expr) {
    return from_def(st, path_id, def);
  }
  let ret = ConstEval { ewp, kind: Kind::Expr };
  match ret.ewp.expr_data(st) {
    ExprData::Subscript { on, idx } => {
      let subscript = from_subscript(st, path_id, *on, *idx);
      Some(subscript.unwrap_or(ret))
    }
    ExprData::Local { body, .. } => get(st, path_id, *body),
    // Id, Import: would have been covered by defs.get above if we knew anything about them
    // Prim, Object, Array, Function: literals are values, they do not evaluate further
    // Error: errors do not evaluate to values
    // ObjectComp, Call, If, BinaryOp, UnaryOp: too tricky, no attempt made to analyze further
    ExprData::Id(_)
    | ExprData::Import { .. }
    | ExprData::Prim(_)
    | ExprData::Object { .. }
    | ExprData::Array(_)
    | ExprData::Function { .. }
    | ExprData::Error(_)
    | ExprData::ObjectComp { .. }
    | ExprData::Call { .. }
    | ExprData::If { .. }
    | ExprData::BinaryOp { .. }
    | ExprData::UnaryOp { .. } => Some(ret),
  }
}

fn from_def(st: &St, path_id: PathId, def: Def) -> Option<ConstEval> {
  match def {
    Def::LocalBind(expr, idx) => {
      let ewp = ExprWithPath { path_id, expr };
      let local = from_local(st, path_id, Some(expr), idx);
      Some(local.unwrap_or(ConstEval { ewp, kind: Kind::LocalBind(idx) }))
    }
    Def::Builtin => None,
    Def::ObjectCompId(expr) => {
      let ewp = ExprWithPath { path_id, expr };
      Some(ConstEval { ewp, kind: Kind::ObjectCompId })
    }
    Def::FunctionParam(expr, idx) => {
      let ewp = ExprWithPath { path_id, expr };
      Some(ConstEval { ewp, kind: Kind::FunctionParam(idx) })
    }
    Def::Import(path_id) => get(st, path_id, st.files[&path_id].top),
  }
}

fn from_local(st: &St, path_id: PathId, expr: Expr, idx: usize) -> Option<ConstEval> {
  let ewp = ExprWithPath { path_id, expr: expr? };
  let ExprData::Local { binds, .. } = ewp.expr_data(st) else { return None };
  let &(_, expr) = binds.get(idx)?;
  get(st, path_id, expr)
}

fn from_subscript(st: &St, path_id: PathId, on: Expr, idx: Expr) -> Option<ConstEval> {
  let on = get(st, path_id, on)?;
  let idx = get(st, path_id, idx)?;
  if !matches!(on.kind, Kind::Expr) || !matches!(idx.kind, Kind::Expr) {
    return None;
  }
  let ExprData::Object { fields, .. } = on.ewp.expr_data(st) else { return None };
  let ExprData::Prim(Prim::String(idx)) = idx.ewp.expr_data(st) else { return None };
  for &(key, _, val) in fields {
    let key = get(st, on.ewp.path_id, key)?;
    if !matches!(key.kind, Kind::Expr) {
      continue;
    }
    let ExprData::Prim(Prim::String(key)) = key.ewp.expr_data(st) else {
      continue;
    };
    if key == idx {
      return get(st, on.ewp.path_id, val);
    }
  }
  None
}
