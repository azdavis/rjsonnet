//! Checking function calls.

use crate::{error, st, ty};
use jsonnet_expr::{Expr, ExprMust, Id};
use rustc_hash::FxHashMap;

pub(crate) fn get(
  st: &mut st::St<'_>,
  expr: ExprMust,
  fn_expr: Expr,
  fn_ty: ty::Ty,
  positional: &[(Expr, ty::Ty)],
  named: &FxHashMap<Id, (Expr, ty::Ty)>,
) -> ty::Ty {
  match st.data(fn_ty).clone() {
    ty::Data::Fn(ty::Fn::Regular(fn_data)) => {
      get_regular(st, expr, fn_expr, &fn_data, positional, named.clone())
    }
    ty::Data::Fn(ty::Fn::Std(std_fn)) => {
      log::warn!("TODO: get std call {std_fn}");
      ty::Ty::ANY
    }
    ty::Data::Union(tys) => {
      // must be compatible with ALL of the union parts.
      let ret_tys = tys.into_iter().map(|fn_ty| get(st, expr, fn_expr, fn_ty, positional, named));
      let ret_ty = ty::Data::Union(ret_tys.collect());
      st.get_ty(ret_ty)
    }
    ty::Data::Prim(ty::Prim::Any) => ty::Ty::ANY,
    ty::Data::Prim(_) | ty::Data::Array(_) | ty::Data::Object(_) => {
      st.err(expr, error::Kind::CallNonFn(fn_ty));
      ty::Ty::ANY
    }
  }
}

fn get_regular(
  st: &mut st::St<'_>,
  expr: jsonnet_expr::Idx<jsonnet_expr::ExprData>,
  fn_expr: Expr,
  fn_data: &ty::RegularFn,
  positional: &[(Expr, ty::Ty)],
  mut named: FxHashMap<Id, (Expr, ty::Ty)>,
) -> ty::Ty {
  let positional_iter = fn_data.params.iter().zip(positional.iter());
  for (param, &(arg, ty)) in positional_iter {
    st.unify(arg.unwrap_or(expr), param.ty, ty);
  }
  for param in fn_data.params.iter().skip(positional.len()) {
    match named.remove(&param.id) {
      Some((arg, ty)) => st.unify(arg.unwrap_or(expr), param.ty, ty),
      None => {
        if param.required {
          st.err(fn_expr.unwrap_or(expr), error::Kind::MissingArgument(param.id, param.ty));
        }
      }
    }
  }
  for (idx, &(arg, _)) in positional.iter().enumerate().skip(fn_data.params.len()) {
    st.err(arg.unwrap_or(expr), error::Kind::ExtraPositionalArgument(idx + 1));
  }
  for (id, (arg, _)) in named {
    st.err(arg.unwrap_or(expr), error::Kind::ExtraNamedArgument(id));
  }
  fn_data.ret
}
