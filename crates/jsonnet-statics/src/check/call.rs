//! Checking function calls.

use crate::{error, st};
use always::always;
use jsonnet_expr::{Expr, ExprMust, Id, StdFn, Str};
use jsonnet_ty as ty;
use rustc_hash::FxHashMap;
use std::collections::BTreeMap;

/// NOTE: the arguments have already been type-checked.
pub(crate) fn get(
  st: &mut st::St<'_>,
  expr: ExprMust,
  fn_expr: Expr,
  fn_ty: ty::Ty,
  pos_args: &[(Expr, ty::Ty)],
  named_args: &FxHashMap<Id, (Expr, ty::Ty)>,
) -> ty::Ty {
  match st.tys.data(fn_ty).clone() {
    ty::Data::Fn(ty::Fn::Regular(fn_data)) => {
      get_regular(st, expr, fn_expr, &fn_data.params, pos_args, named_args.clone(), &mut ignore);
      fn_data.ret
    }
    ty::Data::Fn(ty::Fn::Std(std_fn)) => {
      let sig = ty::StdFnSig::get(std_fn);
      let mut params = FxHashMap::<Id, (ExprMust, ty::Ty)>::default();
      let named_args = named_args.clone();
      get_regular(st, expr, fn_expr, sig.params, pos_args, named_args, &mut |id, expr, ty| {
        params.insert(id, (expr, ty));
      });
      maybe_extra_checks(st, std_fn, &params).unwrap_or(sig.ret)
    }
    ty::Data::Fn(ty::Fn::Hof(_)) => {
      always!(false, "should never call a HOF");
      ty::Ty::ANY
    }
    ty::Data::Union(tys) => {
      // must be compatible with ALL of the union parts.
      let ret_tys =
        tys.into_iter().map(|fn_ty| get(st, expr, fn_expr, fn_ty, pos_args, named_args));
      let ret_ty = ty::Data::Union(ret_tys.collect());
      st.tys.get(ret_ty)
    }
    ty::Data::Prim(ty::Prim::Any) | ty::Data::Fn(ty::Fn::Unknown) => ty::Ty::ANY,
    ty::Data::Prim(_) | ty::Data::Array(_) | ty::Data::Object(_) => {
      st.err(expr, error::Kind::Invalid(fn_ty, error::Invalid::Call));
      ty::Ty::ANY
    }
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

fn ignore(_: Id, _: ExprMust, _: ty::Ty) {}

#[expect(clippy::too_many_lines)]
fn maybe_extra_checks(
  st: &mut st::St<'_>,
  std_fn: StdFn,
  params: &FxHashMap<Id, (ExprMust, ty::Ty)>,
) -> Option<ty::Ty> {
  match std_fn {
    StdFn::length => {
      let &(expr, ty) = params.get(&Id::x)?;
      if !length_ok(st, ty) {
        st.err(expr, error::Kind::Invalid(ty, error::Invalid::Length));
      }
      None
    }
    StdFn::objectValues | StdFn::objectValuesAll => {
      let val_ty = object_values(st, params)?;
      Some(st.tys.get(ty::Data::Array(val_ty)))
    }
    StdFn::objectKeysValues | StdFn::objectKeysValuesAll => {
      let val_ty = object_values(st, params)?;
      let obj = ty::Object {
        known: BTreeMap::from([(Str::key, ty::Ty::STRING), (Str::value, val_ty)]),
        has_unknown: false,
      };
      let elem = st.tys.get(ty::Data::Object(obj));
      Some(st.tys.get(ty::Data::Array(elem)))
    }
    StdFn::map => {
      let &(_, func_ty) = params.get(&Id::func)?;
      let &(arr_expr, arr_ty) = params.get(&Id::arr)?;
      check_map(st, func_ty, arr_expr, arr_ty)
    }
    StdFn::filterMap => {
      let &(_, map_func_ty) = params.get(&Id::map_func)?;
      let &(arr_expr, arr_ty) = params.get(&Id::arr)?;
      check_map(st, map_func_ty, arr_expr, arr_ty)
    }
    StdFn::mod_ => {
      let &(_, lhs_ty) = params.get(&Id::a)?;
      let &(rhs_expr, rhs_ty) = params.get(&Id::b)?;
      match st.tys.data(lhs_ty) {
        ty::Data::Prim(ty::Prim::String) => {
          // NOTE: do NOT unify rhs_ty against `any[]`, because it is permitted to format just one
          // argument without the wrapping array.
          Some(ty::Ty::STRING)
        }
        ty::Data::Prim(ty::Prim::Number) => {
          st.unify(rhs_expr, ty::Ty::NUMBER, rhs_ty);
          Some(ty::Ty::NUMBER)
        }
        _ => None,
      }
    }
    StdFn::join => {
      let &(_, sep_ty) = params.get(&Id::sep)?;
      let &(arr_expr, arr_ty) = params.get(&Id::arr)?;
      // TODO handle unions
      let &ty::Data::Array(elem_ty) = st.tys.data(arr_ty) else { return None };
      st.unify(arr_expr, ty::Ty::STRING_OR_ARRAY_ANY, elem_ty);
      st.unify(arr_expr, sep_ty, elem_ty);
      Some(elem_ty)
    }
    StdFn::reverse | StdFn::sort | StdFn::uniq | StdFn::set | StdFn::filter => {
      let &(_, arr_ty) = params.get(&Id::arr)?;
      Some(arr_ty)
    }
    StdFn::assertEqual => {
      let &(_, lhs_ty) = params.get(&Id::a)?;
      let &(rhs_expr, rhs_ty) = params.get(&Id::b)?;
      st.unify(rhs_expr, lhs_ty, rhs_ty);
      None
    }
    StdFn::equals => {
      let &(_, lhs_ty) = params.get(&Id::x)?;
      let &(rhs_expr, rhs_ty) = params.get(&Id::y)?;
      st.unify(rhs_expr, lhs_ty, rhs_ty);
      None
    }
    StdFn::makeArray => {
      let &(_, func_ty) = params.get(&Id::func)?;
      // TODO handle unions
      let ty::Data::Fn(func) = st.tys.data(func_ty) else { return None };
      let (_, ret) = func.parts();
      Some(st.tys.get(ty::Data::Array(ret)))
    }
    StdFn::slice => {
      let &(_, indexable_ty) = params.get(&Id::indexable)?;
      matches!(st.tys.data(indexable_ty), ty::Data::Prim(ty::Prim::String) | ty::Data::Array(_))
        .then_some(indexable_ty)
    }
    StdFn::repeat => {
      let &(_, what_ty) = params.get(&Id::what)?;
      matches!(st.tys.data(what_ty), ty::Data::Prim(ty::Prim::String) | ty::Data::Array(_))
        .then_some(what_ty)
    }
    StdFn::member => {
      let &(_, arr_ty) = params.get(&Id::arr)?;
      let &(x_expr, x_ty) = params.get(&Id::x)?;
      match st.tys.data(arr_ty) {
        ty::Data::Prim(ty::Prim::String) => {
          st.unify(x_expr, ty::Ty::STRING, x_ty);
        }
        ty::Data::Array(elem_ty) => {
          st.unify(x_expr, *elem_ty, x_ty);
        }
        _ => {}
      }
      None
    }
    StdFn::count => {
      let &(_, arr_ty) = params.get(&Id::arr)?;
      let &(x_expr, x_ty) = params.get(&Id::x)?;
      if let ty::Data::Array(elem_ty) = st.tys.data(arr_ty) {
        st.unify(x_expr, *elem_ty, x_ty);
      }
      None
    }
    StdFn::find => {
      let &(value_expr, value_ty) = params.get(&Id::value)?;
      let &(_, arr_ty) = params.get(&Id::arr)?;
      if let ty::Data::Array(elem_ty) = st.tys.data(arr_ty) {
        st.unify(value_expr, *elem_ty, value_ty);
      }
      None
    }
    StdFn::trace => {
      let &(_, rest_ty) = params.get(&Id::rest)?;
      Some(rest_ty)
    }
    StdFn::foldl | StdFn::foldr => {
      let &(_, func_ty) = params.get(&Id::func)?;
      let &(arr_expr, arr_ty) = params.get(&Id::arr)?;
      let &(init_expr, init_ty) = params.get(&Id::init)?;
      // TODO handle unions
      let ty::Data::Fn(func) = st.tys.data(func_ty) else { return None };
      let (Some(&[ac_param, x_param]), func_ret) = func.parts() else { return None };
      st.unify(init_expr, ac_param.ty, init_ty);
      st.unify(init_expr, func_ret, init_ty);
      let want_arr_ty = st.tys.get(ty::Data::Array(x_param.ty));
      st.unify(arr_expr, want_arr_ty, arr_ty);
      Some(st.tys.get(ty::Data::mk_union([ac_param.ty, func_ret, init_ty])))
    }
    _ => None,
  }
}

fn check_map(
  st: &mut st::St<'_>,
  func_ty: ty::Ty,
  arr_expr: ExprMust,
  arr_ty: ty::Ty,
) -> Option<ty::Ty> {
  // TODO handle unions
  let ty::Data::Fn(func) = st.tys.data(func_ty) else { return None };
  // NOTE no need to emit error when not 1 param, covered by unify with Hof
  let (Some(&[func_param]), func_ret_ty) = func.parts() else { return None };
  let param_arr_ty = st.tys.get(ty::Data::Array(func_param.ty));
  st.unify(arr_expr, param_arr_ty, arr_ty);
  Some(st.tys.get(ty::Data::Array(func_ret_ty)))
}

fn length_ok(st: &st::St<'_>, ty: ty::Ty) -> bool {
  match st.tys.data(ty) {
    ty::Data::Prim(prim) => match prim {
      ty::Prim::Any | ty::Prim::String => true,
      ty::Prim::True | ty::Prim::False | ty::Prim::Null | ty::Prim::Number => false,
    },
    ty::Data::Array(_) | ty::Data::Object(_) | ty::Data::Fn(_) => true,
    ty::Data::Union(tys) => tys.iter().all(|&ty| length_ok(st, ty)),
  }
}

fn object_values(
  st: &mut st::St<'_>,
  params: &FxHashMap<Id, (ExprMust, ty::Ty)>,
) -> Option<ty::Ty> {
  let &(_, ty) = params.get(&Id::o)?;
  let mut ac = ty::Union::new();
  object_values_inner(st, ty, &mut ac);
  Some(st.tys.get(ty::Data::Union(ac)))
}

/// TODO case on whether we are considering hidden fields?
fn object_values_inner(st: &st::St<'_>, ty: ty::Ty, ac: &mut ty::Union) {
  match st.tys.data(ty) {
    ty::Data::Prim(_) | ty::Data::Array(_) | ty::Data::Fn(_) => {}
    ty::Data::Object(obj) => {
      if obj.has_unknown {
        ac.insert(ty::Ty::ANY);
      } else {
        ac.extend(obj.known.values().copied());
      }
    }
    ty::Data::Union(tys) => {
      for &ty in tys {
        object_values_inner(st, ty, ac);
      }
    }
  }
}
