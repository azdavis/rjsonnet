//! Try to "evaluate" without actually evaluating, using statically-known information.

use crate::{st::St, util::Real};
use jsonnet_expr::def::{self, Def};
use jsonnet_expr::{Expr, ExprData, ExprMust, Prim};
use paths::PathId;
use rustc_hash::FxHashSet;

#[derive(Debug)]
pub(crate) enum ConstEval {
  /// A real result - somewhere in user code.
  Real(Real),
  /// The std lib, perhaps narrowed down by selecting a field off of it.
  Std(Option<jsonnet_expr::StdField>),
}

impl From<Real> for ConstEval {
  fn from(r: Real) -> Self {
    Self::Real(r)
  }
}

/// Get some approximate info about this expr, notably, where it approximately was defined.
pub(crate) fn get<F>(st: &mut St, fs: &F, path_id: PathId, expr: Expr) -> Option<ConstEval>
where
  F: Sync + paths::FileSystem,
{
  let mut seen = Seen::default();
  from_expr(st, &mut seen, fs, path_id, expr)
}

type Seen = FxHashSet<(PathId, ExprMust)>;

fn from_expr<F>(
  st: &mut St,
  seen: &mut Seen,
  fs: &F,
  path_id: PathId,
  expr: Expr,
) -> Option<ConstEval>
where
  F: Sync + paths::FileSystem,
{
  let expr = expr?;
  if !seen.insert((path_id, expr)) {
    log::warn!("stop infinite recursion: {path_id:?} {expr:?}");
    return None;
  }
  let arts = st.get_file_artifacts(fs, path_id).ok()?;
  if let Some(&def) = arts.defs.get(&expr) {
    return from_def(st, seen, fs, path_id, def);
  }
  let ret = Real { path_id, expr, kind: None };
  let file = st.get_file_expr(fs, path_id).ok()?;
  match file.ar[expr] {
    ExprData::Subscript { on, idx } => {
      let subscript = from_subscript(st, seen, fs, path_id, on, idx);
      Some(subscript.unwrap_or(ret.into()))
    }
    ExprData::Local { body, .. } => from_expr(st, seen, fs, path_id, body),
    // if one of the branches diverges, can choose the other one
    ExprData::If { cond: _, yes: Some(yes), no: Some(no) } => {
      if matches!(&file.ar[yes], ExprData::Error(_)) {
        from_expr(st, seen, fs, path_id, Some(no))
      } else if matches!(&file.ar[no], ExprData::Error(_)) {
        from_expr(st, seen, fs, path_id, Some(yes))
      } else {
        Some(ret.into())
      }
    }
    ExprData::SubstOuter(e) => from_expr(st, seen, fs, path_id, e),
    // Id, Import: would have been covered by defs.get above if we knew anything about them
    // Prim, Object, Array, Function: literals are values, they do not evaluate further
    // Error: errors do not evaluate to values
    // ObjectComp, Call, If, BinaryOp, UnaryOp: too tricky, no attempt made to analyze further
    ExprData::Id(_)
    | ExprData::Import { .. }
    | ExprData::Prim(_)
    | ExprData::Object { .. }
    | ExprData::Array(_)
    | ExprData::Fn { .. }
    | ExprData::Error(_)
    | ExprData::ObjectComp { .. }
    | ExprData::Call { .. }
    | ExprData::If { .. }
    | ExprData::BinOp { .. }
    | ExprData::UnOp { .. } => Some(ret.into()),
  }
}

fn from_def<F>(st: &mut St, seen: &mut Seen, fs: &F, path_id: PathId, def: Def) -> Option<ConstEval>
where
  F: Sync + paths::FileSystem,
{
  match def {
    Def::Expr(ed) => {
      let local = if let def::ExprDefKind::Multi(idx, def::ExprDefKindMulti::LocalBind) = ed.kind {
        from_local(st, seen, fs, path_id, Some(ed.expr), idx)
      } else {
        None
      };
      Some(local.unwrap_or(Real { path_id, expr: ed.expr, kind: Some(ed.kind) }.into()))
    }
    Def::Std => Some(ConstEval::Std(None)),
    Def::KwIdent => None,
    Def::Import(path_id) => {
      let top = st.get_file_expr(fs, path_id).ok()?.top;
      from_expr(st, seen, fs, path_id, top)
    }
  }
}

fn from_local<F>(
  st: &mut St,
  seen: &mut Seen,
  fs: &F,
  path_id: PathId,
  expr: Expr,
  idx: usize,
) -> Option<ConstEval>
where
  F: Sync + paths::FileSystem,
{
  let expr = expr?;
  let file = st.get_file_expr(fs, path_id).ok()?;
  let ExprData::Local { binds, .. } = &file.ar[expr] else { return None };
  let &(_, expr) = binds.get(idx)?;
  from_expr(st, seen, fs, path_id, expr)
}

fn from_subscript<F>(
  st: &mut St,
  seen: &mut Seen,
  fs: &F,
  path_id: PathId,
  on: Expr,
  idx: Expr,
) -> Option<ConstEval>
where
  F: Sync + paths::FileSystem,
{
  let ConstEval::Real(idx) = from_expr(st, seen, fs, path_id, idx)? else { return None };
  if idx.kind.is_some() {
    return None;
  }
  let idx = {
    let file = st.get_file_expr(fs, idx.path_id).ok()?;
    let ExprData::Prim(Prim::String(idx)) = file.ar[idx.expr] else { return None };
    idx
  };
  let on = match from_expr(st, seen, fs, path_id, on)? {
    ConstEval::Std(None) => {
      let field = jsonnet_expr::StdField::try_from(idx).ok()?;
      return Some(ConstEval::Std(Some(field)));
    }
    ConstEval::Std(Some(_)) => return None,
    ConstEval::Real(on) => on,
  };
  if on.kind.is_some() {
    return None;
  }
  let fields = {
    let file = st.get_file_expr(fs, on.path_id).ok()?;
    let ExprData::Object { fields, .. } = &file.ar[on.expr] else { return Some(on.into()) };
    fields.clone()
  };
  for field in fields {
    let field_key = from_expr(st, seen, fs, on.path_id, field.key);
    let Some(ConstEval::Real(key)) = field_key else { continue };
    if key.kind.is_some() {
      continue;
    }
    let Ok(file) = st.get_file_expr(fs, key.path_id) else { continue };
    let ExprData::Prim(Prim::String(key)) = &file.ar[key.expr] else { continue };
    if *key == idx {
      return from_expr(st, seen, fs, on.path_id, field.val);
    }
  }
  Some(on.into())
}
