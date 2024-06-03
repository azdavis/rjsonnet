//! Try to "evaluate" without actually evaluating, using statically-known information.

use crate::st::St;
use jsonnet_expr::def::{self, Def};
use jsonnet_expr::{Expr, ExprData, ExprMust, Prim};
use paths::PathId;

#[derive(Debug)]
pub(crate) enum ConstEval {
  /// A real result - somewhere in user code.
  Real(Real),
  /// The std lib, perhaps narrowed down by selecting a field off of it.
  Std(Option<jsonnet_expr::Str>),
}

/// Somewhere in "real", user-written code.
#[derive(Debug)]
pub(crate) struct Real {
  pub(crate) path_id: PathId,
  pub(crate) expr: ExprMust,
  pub(crate) kind: Option<def::ExprDefKind>,
}

impl From<Real> for ConstEval {
  fn from(r: Real) -> Self {
    Self::Real(r)
  }
}

/// Get some approximate info about this expr, notably, where it approximately was defined.
pub(crate) fn get<F>(st: &mut St, fs: &F, path_id: PathId, expr: Expr) -> Option<ConstEval>
where
  F: paths::FileSystem,
{
  let expr = expr?;
  let arts = st.get_file_artifacts(fs, path_id).ok()?;
  if let Some(&def) = arts.defs.get(&expr) {
    return from_def(st, fs, path_id, def);
  }
  let ret = Real { path_id, expr, kind: None };
  let file = st.get_file_expr(fs, path_id).ok()?;
  match file.expr_ar[expr].clone() {
    ExprData::Subscript { on, idx } => {
      let subscript = from_subscript(st, fs, path_id, on, idx);
      Some(subscript.unwrap_or(ret.into()))
    }
    ExprData::Local { body, .. } => get(st, fs, path_id, body),
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
    | ExprData::UnaryOp { .. } => Some(ret.into()),
  }
}

fn from_def<F>(st: &mut St, fs: &F, path_id: PathId, def: Def) -> Option<ConstEval>
where
  F: paths::FileSystem,
{
  match def {
    Def::Expr(expr, kind) => {
      let local = if let def::ExprDefKind::LocalBind(idx) = kind {
        from_local(st, fs, path_id, Some(expr), idx)
      } else {
        None
      };
      Some(local.unwrap_or(Real { path_id, expr, kind: Some(kind) }.into()))
    }
    Def::Std => Some(ConstEval::Std(None)),
    Def::KwIdent => None,
    Def::Import(path_id) => {
      let top = st.get_file_expr(fs, path_id).ok()?.top;
      get(st, fs, path_id, top)
    }
  }
}

fn from_local<F>(st: &mut St, fs: &F, path_id: PathId, expr: Expr, idx: usize) -> Option<ConstEval>
where
  F: paths::FileSystem,
{
  let expr = expr?;
  let file = st.get_file_expr(fs, path_id).ok()?;
  let ExprData::Local { binds, .. } = &file.expr_ar[expr] else { return None };
  let &(_, expr) = binds.get(idx)?;
  get(st, fs, path_id, expr)
}

fn from_subscript<F>(st: &mut St, fs: &F, path_id: PathId, on: Expr, idx: Expr) -> Option<ConstEval>
where
  F: paths::FileSystem,
{
  let ConstEval::Real(idx) = get(st, fs, path_id, idx)? else { return None };
  if idx.kind.is_some() {
    return None;
  }
  let idx = {
    let file = st.get_file_expr(fs, idx.path_id).ok()?;
    let ExprData::Prim(Prim::String(idx)) = &file.expr_ar[idx.expr] else { return None };
    idx.clone()
  };
  let on = match get(st, fs, path_id, on)? {
    ConstEval::Std(None) => return Some(ConstEval::Std(Some(idx))),
    ConstEval::Std(Some(_)) => return None,
    ConstEval::Real(on) => on,
  };
  if on.kind.is_some() {
    return None;
  }
  let fields = {
    let file = st.get_file_expr(fs, on.path_id).ok()?;
    let ExprData::Object { fields, .. } = &file.expr_ar[on.expr] else { return None };
    fields.clone()
  };
  for field in fields {
    let Some(ConstEval::Real(key)) = get(st, fs, on.path_id, field.key) else { continue };
    if key.kind.is_some() {
      continue;
    }
    let Ok(file) = st.get_file_expr(fs, key.path_id) else { continue };
    let ExprData::Prim(Prim::String(key)) = &file.expr_ar[key.expr] else {
      continue;
    };
    if *key == idx {
      return get(st, fs, on.path_id, field.val);
    }
  }
  None
}
