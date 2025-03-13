//! Checking function calls.

use crate::{error, flow, st};
use always::always;
use jsonnet_expr::{Expr, ExprArena, ExprData, ExprMust, Id, Prim, StdFn, Str};
use jsonnet_format_string::{Code, ConvType};
use jsonnet_ty as ty;
use rustc_hash::FxHashMap;
use std::collections::BTreeMap;

/// NOTE: the arguments have already been type-checked.
pub(crate) fn get(
  st: &mut st::St<'_>,
  ar: &ExprArena,
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
      maybe_extra_checks(st, ar, std_fn, &params).unwrap_or(sig.ret)
    }
    ty::Data::Fn(ty::Fn::StdParam(_)) => {
      always!(false, "should never call a StdParam");
      ty::Ty::ANY
    }
    ty::Data::Union(tys) => {
      // must be compatible with ALL of the union parts.
      let ret_tys =
        tys.into_iter().map(|fn_ty| get(st, ar, expr, fn_expr, fn_ty, pos_args, named_args));
      let ret_ty = ty::Data::Union(ret_tys.collect());
      st.tys.get(ret_ty)
    }
    ty::Data::Prim(ty::Prim::Any) | ty::Data::Fn(ty::Fn::Unknown) => ty::Ty::ANY,
    ty::Data::Prim(_) | ty::Data::Array(_) | ty::Data::Tuple(_) | ty::Data::Object(_) => {
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
          st.err(fn_expr.unwrap_or(expr), error::Kind::MissingArg(param.id, param.ty));
        }
      }
    }
  }
  for (idx, &(arg, _)) in pos_args.iter().enumerate().skip(params.len()) {
    st.err(arg.unwrap_or(expr), error::Kind::ExtraPositionalArg(idx + 1));
  }
  for (id, (arg, _)) in named_args {
    st.err(arg.unwrap_or(expr), error::Kind::ExtraNamedArg(id));
  }
}

fn ignore(_: Id, _: ExprMust, _: ty::Ty) {}

#[expect(clippy::too_many_lines)]
fn maybe_extra_checks(
  st: &mut st::St<'_>,
  ar: &ExprArena,
  std_fn: StdFn,
  params: &FxHashMap<Id, (ExprMust, ty::Ty)>,
) -> Option<ty::Ty> {
  match std_fn {
    StdFn::length => {
      let &(expr, ty) = params.get(&Id::x)?;
      if !length_ok(&st.tys, ty) {
        st.err(expr, error::Kind::Invalid(ty, error::Invalid::Length));
      }
      None
    }
    StdFn::objectValues | StdFn::objectValuesAll => {
      let elem = object_values(&mut st.tys, params)?;
      Some(st.tys.get(ty::Data::Array(ty::Array::new(elem))))
    }
    StdFn::objectKeysValues | StdFn::objectKeysValuesAll => {
      let val_ty = object_values(&mut st.tys, params)?;
      let obj = ty::Object {
        known: BTreeMap::from([(Str::key, ty::Ty::STRING), (Str::value, val_ty)]),
        has_unknown: false,
      };
      let elem = st.tys.get(ty::Data::Object(obj));
      Some(st.tys.get(ty::Data::Array(ty::Array::new(elem))))
    }
    StdFn::filter => {
      let &(func, _) = params.get(&Id::func)?;
      let &(_, arr_ty) = params.get(&Id::arr)?;
      check_filter(st, ar, func, arr_ty)
    }
    StdFn::map => {
      let &(_, func_ty) = params.get(&Id::func)?;
      let &(arr_expr, arr_ty) = params.get(&Id::arr)?;
      check_map(st, func_ty, arr_expr, arr_ty)
    }
    StdFn::filterMap => {
      let &(filter_func, _) = params.get(&Id::filter_func)?;
      let &(_, map_func_ty) = params.get(&Id::map_func)?;
      let &(arr_expr, arr_ty) = params.get(&Id::arr)?;
      let arr_ty = check_filter(st, ar, filter_func, arr_ty).unwrap_or(arr_ty);
      check_map(st, map_func_ty, arr_expr, arr_ty)
    }
    StdFn::flatMap => {
      let &(func_expr, func_ty) = params.get(&Id::func)?;
      let &(_, arr_ty) = params.get(&Id::arr)?;
      let (want_param, want_ret) = if arr_ty == ty::Ty::STRING {
        // no char type
        (ty::Ty::STRING, ty::Ty::STRING)
      } else {
        let arr = array_ty(&mut st.tys, arr_ty)?;
        (arr.elem, st.tys.get(ty::Data::Array(arr)))
      };
      let ty::Data::Fn(func) = st.tys.data(func_ty) else { return None };
      if let Some((&[param], ret_ty)) = func.parts() {
        st.unify(func_expr, want_param, param.ty);
        st.unify(func_expr, want_ret, ret_ty);
      }
      Some(want_ret)
    }
    StdFn::mod_ => {
      let &(lhs_expr, lhs_ty) = params.get(&Id::a)?;
      let &(rhs_expr, rhs_ty) = params.get(&Id::b)?;
      match st.tys.data(lhs_ty) {
        ty::Data::Prim(ty::Prim::String) => {
          if let ExprData::Prim(Prim::String(s)) = ar[lhs_expr] {
            let s = st.str_ar.get(s);
            check_format(st, lhs_expr, s, rhs_ty);
          }
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
      let arr = array_ty(&mut st.tys, arr_ty)?;
      st.unify(arr_expr, ty::Ty::STRING_OR_ARRAY, arr.elem);
      st.unify(arr_expr, arr.elem, sep_ty);
      Some(if arr.elem == ty::Ty::STRING {
        ty::Ty::STRING
      } else {
        let elem = array_ty(&mut st.tys, arr.elem)?.elem;
        st.tys.get(ty::Data::Array(ty::Array::new(elem)))
      })
    }
    StdFn::reverse => {
      let &(_, arr_ty) = params.get(&Id::arr)?;
      let arr = array_ty(&mut st.tys, arr_ty)?;
      // might not be a set anymore (if it was before)
      Some(st.tys.get(ty::Data::Array(ty::Array::new(arr.elem))))
    }
    StdFn::sort | StdFn::uniq => {
      let &(_, arr_ty) = params.get(&Id::arr)?;
      // is still a set (if it was before)
      Some(arr_ty)
    }
    StdFn::set => {
      let &(_, arr_ty) = params.get(&Id::arr)?;
      let arr = array_ty(&mut st.tys, arr_ty)?;
      Some(st.tys.get(ty::Data::Array(ty::Array::set(arr.elem))))
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
      // NOTE no attempt to handle union of fn tys
      let ty::Data::Fn(func) = st.tys.data(func_ty) else { return None };
      let ret = func.parts().map_or(ty::Ty::ANY, |(_, r)| r);
      Some(st.tys.get(ty::Data::Array(ty::Array::new(ret))))
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
        ty::Data::Array(arr) => {
          st.unify(x_expr, arr.elem, x_ty);
        }
        _ => {}
      }
      None
    }
    StdFn::count => {
      let &(_, arr_ty) = params.get(&Id::arr)?;
      let &(x_expr, x_ty) = params.get(&Id::x)?;
      if let ty::Data::Array(arr) = st.tys.data(arr_ty) {
        st.unify(x_expr, arr.elem, x_ty);
      }
      None
    }
    StdFn::find => {
      let &(value_expr, value_ty) = params.get(&Id::value)?;
      let &(_, arr_ty) = params.get(&Id::arr)?;
      if let ty::Data::Array(arr) = st.tys.data(arr_ty) {
        st.unify(value_expr, arr.elem, value_ty);
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
      // NOTE no attempt to handle union of fn tys
      let ty::Data::Fn(func) = st.tys.data(func_ty) else { return None };
      let Some((&[ac_param, x_param], func_ret)) = func.parts() else { return None };
      st.unify(init_expr, ac_param.ty, init_ty);
      st.unify(init_expr, func_ret, init_ty);
      let want_arr_ty = st.tys.get(ty::Data::Array(ty::Array::new(x_param.ty)));
      st.unify(arr_expr, want_arr_ty, arr_ty);
      Some(st.tys.get(ty::Data::mk_union([ac_param.ty, func_ret, init_ty])))
    }
    StdFn::prune => {
      let &(_, ty) = params.get(&Id::a)?;
      // special cases:
      //
      // - std.prune(null) == null
      // - std.prune({}) == {}
      //
      // in both cases, the input type is the output type (not pruned).
      let is_special_case = match st.tys.data(ty) {
        ty::Data::Prim(ty::Prim::Null) => true,
        ty::Data::Object(obj) => !obj.has_unknown && obj.known.is_empty(),
        _ => false,
      };
      let ret = if is_special_case { ty } else { prune(&mut st.tys, ty) };
      Some(ret)
    }
    StdFn::format => {
      let &(s_expr, _) = params.get(&Id::str)?;
      let &(_, ty) = params.get(&Id::vals)?;
      let ExprData::Prim(Prim::String(s)) = ar[s_expr] else { return None };
      let s = st.str_ar.get(s);
      check_format(st, s_expr, s, ty);
      None
    }
    _ => None,
  }
}

fn check_format(st: &mut st::St<'_>, expr: ExprMust, s: &str, ty: ty::Ty) {
  let codes: Vec<_> = match jsonnet_format_string::get(s) {
    Ok(es) => es.into_iter().filter_map(jsonnet_format_string::Elem::into_code).collect(),
    Err(e) => {
      st.err(expr, error::Kind::FormatParseFail(e));
      return;
    }
  };
  match st.tys.data(ty) {
    // TODO handle formatting object
    ty::Data::Prim(ty::Prim::Any) | ty::Data::Array(_) | ty::Data::Object(_) => {}
    ty::Data::Prim(_) | ty::Data::Fn(_) | ty::Data::Union(_) => {
      if codes.len() != 1 {
        st.err(expr, error::Kind::FormatWrongCount(codes.len(), 1));
      }
      if let Some(code) = codes.first() {
        check_format_code(st, expr, code, ty);
      }
    }
    ty::Data::Tuple(tup) => {
      let tup = tup.clone();
      if codes.len() != tup.elems.len() {
        st.err(expr, error::Kind::FormatWrongCount(codes.len(), tup.elems.len()));
      }
      for (code, &ty) in codes.iter().zip(tup.elems.iter()) {
        check_format_code(st, expr, code, ty);
      }
    }
  }
}

fn check_format_code(st: &mut st::St<'_>, expr: ExprMust, code: &Code, ty: ty::Ty) {
  match code.ctype {
    ConvType::D
    | ConvType::O
    | ConvType::X(_)
    | ConvType::E(_)
    | ConvType::F(_)
    | ConvType::G(_) => {
      st.unify(expr, ty::Ty::NUMBER, ty);
    }
    ConvType::C => {
      st.unify(expr, ty::Ty::NUMBER_OR_STRING, ty);
    }
    // anything allowed (except maybe fn?)
    ConvType::S => {}
  }
}

/// recursively remove nulls, known zero-length objects, and known zero-length arrays (tuples) from
/// the ty.
fn prune(tys: &mut ty::MutStore<'_>, ty: ty::Ty) -> ty::Ty {
  match tys.data(ty) {
    ty::Data::Prim(prim) => match prim {
      // remove null
      ty::Prim::Null => ty::Ty::NEVER,
      // leave alone
      ty::Prim::Any | ty::Prim::True | ty::Prim::False | ty::Prim::String | ty::Prim::Number => ty,
    },
    // leave alone
    ty::Data::Fn(_) => ty,
    // just recur; not statically known if array is length zero
    ty::Data::Array(arr) => {
      // probably is sorted even after pruning (based on the definition of pruning), but let's
      // conservatively say it's not.
      let elem = prune(tys, arr.elem);
      let arr = ty::Array::new(elem);
      tys.get(ty::Data::Array(arr))
    }
    ty::Data::Tuple(tup) => {
      // remove empty arrays/tuples
      if tup.elems.is_empty() {
        ty::Ty::NEVER
      } else {
        let mut elems = Vec::<ty::Ty>::new();
        let tup = tup.clone();
        for ty in tup.elems {
          let ty = prune(tys, ty);
          if ty != ty::Ty::NEVER {
            elems.push(ty);
          }
        }
        tys.get(ty::Data::Tuple(ty::Tuple { elems }))
      }
    }
    ty::Data::Object(obj) => {
      // need unique access to tys
      let obj = obj.clone();
      if !obj.has_unknown && obj.known.is_empty() {
        // remove zero-length object
        ty::Ty::NEVER
      } else {
        // recur
        let iter = obj.known.into_iter().filter_map(|(key, ty)| {
          let ty = prune(tys, ty);
          if ty == ty::Ty::NEVER { None } else { Some((key, ty)) }
        });
        let obj = ty::Object { known: iter.collect(), has_unknown: obj.has_unknown };
        tys.get(ty::Data::Object(obj))
      }
    }
    ty::Data::Union(parts) => {
      // need unique access to tys
      let parts = parts.clone();
      let iter = parts.into_iter().map(|ty| prune(tys, ty));
      let u = ty::Data::Union(iter.collect());
      tys.get(u)
    }
  }
}

fn check_map(
  st: &mut st::St<'_>,
  func_ty: ty::Ty,
  arr_expr: ExprMust,
  arr_ty: ty::Ty,
) -> Option<ty::Ty> {
  // NOTE no attempt to handle union of fn tys
  let ty::Data::Fn(func) = st.tys.data(func_ty) else { return None };
  // NOTE no need to emit error when not 1 param, covered by unify
  let Some((&[func_param], func_ret_ty)) = func.parts() else { return None };
  let param_arr_ty = st.tys.get(ty::Data::Array(ty::Array::new(func_param.ty)));
  st.unify(arr_expr, param_arr_ty, arr_ty);
  Some(st.tys.get(ty::Data::Array(ty::Array::new(func_ret_ty))))
}

fn check_filter(
  st: &mut st::St<'_>,
  ar: &ExprArena,
  func: ExprMust,
  arr_ty: ty::Ty,
) -> Option<ty::Ty> {
  let arr = array_ty(&mut st.tys, arr_ty)?;
  let mut elem = arr.elem;
  if let Some(fact) = flow::extract::get_predicate(&st.scope, ar, func) {
    fact.apply_to(&mut st.tys, &mut elem);
  }
  Some(st.tys.get(ty::Data::Array(ty::Array::new(elem))))
}

fn length_ok(tys: &ty::MutStore<'_>, ty: ty::Ty) -> bool {
  match tys.data(ty) {
    ty::Data::Prim(prim) => match prim {
      ty::Prim::Any | ty::Prim::String => true,
      ty::Prim::True | ty::Prim::False | ty::Prim::Null | ty::Prim::Number => false,
    },
    ty::Data::Array(_) | ty::Data::Tuple(_) | ty::Data::Object(_) | ty::Data::Fn(_) => true,
    ty::Data::Union(parts) => parts.iter().all(|&ty| length_ok(tys, ty)),
  }
}

fn object_values(
  tys: &mut ty::MutStore<'_>,
  params: &FxHashMap<Id, (ExprMust, ty::Ty)>,
) -> Option<ty::Ty> {
  let &(_, ty) = params.get(&Id::o)?;
  let mut ac = ty::Union::new();
  object_values_inner(tys, ty, &mut ac);
  Some(tys.get(ty::Data::Union(ac)))
}

fn object_values_inner(tys: &ty::MutStore<'_>, ty: ty::Ty, ac: &mut ty::Union) {
  match tys.data(ty) {
    ty::Data::Prim(_) | ty::Data::Array(_) | ty::Data::Tuple(_) | ty::Data::Fn(_) => {}
    ty::Data::Object(obj) => {
      if obj.has_unknown {
        ac.insert(ty::Ty::ANY);
      } else {
        ac.extend(obj.known.values().copied());
      }
    }
    ty::Data::Union(parts) => {
      for &ty in parts {
        object_values_inner(tys, ty, ac);
      }
    }
  }
}

fn array_ty(tys: &mut ty::MutStore<'_>, arr_ty: ty::Ty) -> Option<ty::Array> {
  match tys.data(arr_ty) {
    ty::Data::Array(arr) => Some(*arr),
    ty::Data::Tuple(tup) => {
      let un: ty::Union = tup.elems.iter().copied().collect();
      Some(ty::Array::new(tys.get(ty::Data::Union(un))))
    }
    ty::Data::Union(parts) => {
      let parts = parts.clone();
      let mut elem = ty::Union::new();
      let mut is_set = true;
      for part in parts {
        let other = array_ty(tys, part)?;
        elem.insert(other.elem);
        is_set = is_set && other.is_set;
      }
      Some(ty::Array { elem: tys.get(ty::Data::Union(elem)), is_set })
    }
    _ => None,
  }
}
