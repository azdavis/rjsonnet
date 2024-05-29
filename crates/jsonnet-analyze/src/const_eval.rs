//! Try to "evaluate" without actually evaluating, using statically-known information.

use crate::St;
use always::always;
use jsonnet_expr::{Expr, ExprData, ExprMust, Prim};
use jsonnet_statics::Def;
use paths::PathId;

#[derive(Debug)]
pub(crate) struct ConstEval {
  pub(crate) path_id: PathId,
  pub(crate) expr: ExprMust,
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
  let expr = expr?;
  let arts = st.get_file_artifacts(path_id)?;
  if let Some(&def) = arts.defs.get(&expr) {
    return from_def(st, path_id, def);
  }
  let ret = ConstEval { path_id, expr, kind: Kind::Expr };
  let Some(file) = st.file_exprs.get(&path_id) else {
    let path = st.display_path_id(path_id);
    always!(false, "no file exprs for {path}");
    return None;
  };
  match file.expr_ar[expr].clone() {
    ExprData::Subscript { on, idx } => {
      let subscript = from_subscript(st, path_id, on, idx);
      Some(subscript.unwrap_or(ret))
    }
    ExprData::Local { body, .. } => get(st, path_id, body),
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
      let local = from_local(st, path_id, Some(expr), idx);
      Some(local.unwrap_or(ConstEval { path_id, expr, kind: Kind::LocalBind(idx) }))
    }
    Def::Builtin => None,
    Def::ObjectCompId(expr) => Some(ConstEval { path_id, expr, kind: Kind::ObjectCompId }),
    Def::FunctionParam(expr, idx) => {
      Some(ConstEval { path_id, expr, kind: Kind::FunctionParam(idx) })
    }
    Def::Import(path_id) => {
      let top = st.get_file_expr(path_id)?.top;
      get(st, path_id, top)
    }
  }
}

fn from_local(st: &St, path_id: PathId, expr: Expr, idx: usize) -> Option<ConstEval> {
  let expr = expr?;
  let file = st.get_file_expr(path_id)?;
  let ExprData::Local { binds, .. } = &file.expr_ar[expr] else { return None };
  let &(_, expr) = binds.get(idx)?;
  get(st, path_id, expr)
}

fn from_subscript(st: &St, path_id: PathId, on: Expr, idx: Expr) -> Option<ConstEval> {
  let on = get(st, path_id, on)?;
  let idx = get(st, path_id, idx)?;
  let (Kind::Expr, Kind::Expr) = (on.kind, idx.kind) else { return None };
  let fields = {
    let file = st.get_file_expr(on.path_id)?;
    let ExprData::Object { fields, .. } = &file.expr_ar[on.expr] else { return None };
    fields.clone()
  };
  let idx = {
    let file = st.get_file_expr(idx.path_id)?;
    let ExprData::Prim(Prim::String(idx)) = &file.expr_ar[idx.expr] else { return None };
    idx.clone()
  };
  for field in fields {
    let key = get(st, on.path_id, field.key)?;
    let Kind::Expr = key.kind else { continue };
    let file = st.get_file_expr(key.path_id)?;
    let ExprData::Prim(Prim::String(key)) = &file.expr_ar[key.expr] else {
      continue;
    };
    if *key == idx {
      return get(st, on.path_id, field.val);
    }
  }
  None
}
