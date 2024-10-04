//! Checking function calls.

use crate::{error, st};
use jsonnet_expr::{Expr, ExprMust, Id, StdFn};
use jsonnet_ty as ty;
use rustc_hash::FxHashMap;

pub(crate) fn get(
  st: &mut st::St<'_>,
  expr: ExprMust,
  fn_expr: Expr,
  fn_ty: ty::Ty,
  pos_args: &[(Expr, ty::Ty)],
  named_args: &FxHashMap<Id, (Expr, ty::Ty)>,
) -> ty::Ty {
  match st.data(fn_ty).clone() {
    ty::Data::Fn(ty::Fn::Regular(fn_data)) => {
      get_regular(st, expr, fn_expr, &fn_data.params, pos_args, named_args.clone(), &mut ignore);
      fn_data.ret
    }
    ty::Data::Fn(ty::Fn::Std(std_fn)) => {
      let sig = ty::StdFnSig::get(std_fn);
      let mut params = FxHashMap::<Id, (ExprMust, ty::Ty)>::default();
      let named_args = named_args.clone();
      // TODO only insert when we need to do extra checks?
      get_regular(st, expr, fn_expr, sig.params, pos_args, named_args, &mut |id, expr, ty| {
        params.insert(id, (expr, ty));
      });
      maybe_extra_checks(st, std_fn, &params).unwrap_or(sig.ret)
    }
    ty::Data::Union(tys) => {
      // must be compatible with ALL of the union parts.
      let ret_tys =
        tys.into_iter().map(|fn_ty| get(st, expr, fn_expr, fn_ty, pos_args, named_args));
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

fn ignore(_: Id, _: ExprMust, _: ty::Ty) {}

fn maybe_extra_checks(
  st: &mut st::St<'_>,
  std_fn: StdFn,
  params: &FxHashMap<Id, (ExprMust, ty::Ty)>,
) -> Option<ty::Ty> {
  match std_fn {
    StdFn::length => {
      let &(expr, ty) = params.get(&Id::x)?;
      if !length_ok(st, ty) {
        st.err(expr, error::Kind::InvalidLength(ty));
      }
      None
    }
    _ => None,
  }
}

fn length_ok(st: &st::St<'_>, ty: ty::Ty) -> bool {
  match st.data(ty) {
    ty::Data::Prim(prim) => match prim {
      ty::Prim::Any | ty::Prim::String => true,
      ty::Prim::True | ty::Prim::False | ty::Prim::Null | ty::Prim::Number => false,
    },
    ty::Data::Array(_) | ty::Data::Object(_) | ty::Data::Fn(_) => true,
    ty::Data::Union(tys) => tys.iter().all(|&ty| length_ok(st, ty)),
  }
}

fn get_regular<F>(
  st: &mut st::St<'_>,
  expr: ExprMust,
  fn_expr: Expr,
  params: &[ty::Param],
  pos_args: &[(Expr, ty::Ty)],
  mut named_args: FxHashMap<Id, (Expr, ty::Ty)>,
  f: &mut F,
) where
  F: FnMut(Id, ExprMust, ty::Ty),
{
  let positional_iter = params.iter().zip(pos_args.iter());
  for (param, &(arg, ty)) in positional_iter {
    let e = arg.unwrap_or(expr);
    st.unify(e, param.ty, ty);
    f(param.id, e, ty);
  }
  for param in params.iter().skip(pos_args.len()) {
    match named_args.remove(&param.id) {
      Some((arg, ty)) => {
        let e = arg.unwrap_or(expr);
        st.unify(e, param.ty, ty);
        f(param.id, e, ty);
      }
      None => {
        if param.required {
          st.err(fn_expr.unwrap_or(expr), error::Kind::MissingArgument(param.id, param.ty));
        }
      }
    }
  }
  for (idx, &(arg, _)) in pos_args.iter().enumerate().skip(params.len()) {
    st.err(arg.unwrap_or(expr), error::Kind::ExtraPositionalArgument(idx + 1));
  }
  for (id, (arg, _)) in named_args {
    st.err(arg.unwrap_or(expr), error::Kind::ExtraNamedArgument(id));
  }
}
