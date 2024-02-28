//! Try to "evaluate" without actually evaluating, using statically-known information.

use jsonnet_expr::{Expr, ExprArena, ExprData, Prim};
use jsonnet_statics::{Def, DefMap};

#[derive(Debug)]
pub(crate) enum ConstEval {
  Expr(Expr),
  Def(Def),
}

pub(crate) fn get_expr(defs: &DefMap, exprs: &ExprArena, expr: Expr) -> ConstEval {
  let Some(expr_must) = expr else { return ConstEval::Expr(None) };
  if let Some(&def) = defs.get(&expr_must) {
    return get_def(defs, exprs, def);
  }
  match &exprs[expr_must] {
    ExprData::Subscript { on, idx } => {
      get_subscript(defs, exprs, *on, *idx).unwrap_or(ConstEval::Expr(expr))
    }
    ExprData::Local { body, .. } => get_expr(defs, exprs, *body),
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
    | ExprData::UnaryOp { .. } => ConstEval::Expr(expr),
  }
}

fn get_def(defs: &DefMap, exprs: &ExprArena, def: Def) -> ConstEval {
  match def {
    Def::LocalBind(expr, idx) => {
      get_local(defs, exprs, Some(expr), idx).unwrap_or(ConstEval::Def(def))
    }
    // Builtin: opaque
    // ObjectCompId: too tricky, no attempt made to analyze further
    // FunctionParam: params can be arbitrary, impossible to analyze further
    // Import: TODO
    Def::Builtin | Def::ObjectCompId(_) | Def::FunctionParam(_, _) | Def::Import(_) => {
      ConstEval::Def(def)
    }
  }
}

fn get_local(defs: &DefMap, exprs: &ExprArena, expr: Expr, idx: usize) -> Option<ConstEval> {
  let expr = expr?;
  let ExprData::Local { binds, .. } = &exprs[expr] else { return None };
  let &(_, expr) = binds.get(idx)?;
  Some(get_expr(defs, exprs, expr))
}

fn get_subscript(defs: &DefMap, exprs: &ExprArena, on: Expr, idx: Expr) -> Option<ConstEval> {
  let ConstEval::Expr(Some(on)) = get_expr(defs, exprs, on) else { return None };
  let ConstEval::Expr(Some(idx)) = get_expr(defs, exprs, idx) else { return None };
  let ExprData::Object { fields, .. } = &exprs[on] else { return None };
  let ExprData::Prim(Prim::String(idx)) = &exprs[idx] else { return None };
  for &(key, _, val) in fields {
    let ConstEval::Expr(Some(key)) = get_expr(defs, exprs, key) else { continue };
    let ExprData::Prim(Prim::String(key)) = &exprs[key] else { continue };
    if key == idx {
      return Some(get_expr(defs, exprs, val));
    }
  }
  None
}
