//! Check expressions for static validity: variables are in scope, types match up, etc.

mod call;

use crate::{error, flow, st, suggestion};
use always::always;
use jsonnet_expr::{def, BinaryOp, Expr, ExprArena, ExprData, ExprMust, Id, Prim, UnaryOp};
use jsonnet_ty as ty;
use rustc_hash::{FxHashMap, FxHashSet};

/// NOTE: don't return early from this except in the degenerate case where the `expr` was `None`.
/// This is so we can insert the expr's type into the `St` at the end.
#[expect(clippy::too_many_lines)]
pub(crate) fn get(st: &mut st::St<'_>, ar: &ExprArena, expr: Expr) -> ty::Ty {
  let Some(expr) = expr else { return ty::Ty::ANY };
  let ret = match &ar[expr] {
    ExprData::Prim(prim) => match prim {
      Prim::Null => ty::Ty::NULL,
      Prim::Bool(b) => {
        if *b {
          ty::Ty::TRUE
        } else {
          ty::Ty::FALSE
        }
      }
      Prim::String(_) => ty::Ty::STRING,
      Prim::Number(_) => ty::Ty::NUMBER,
    },
    ExprData::Object { binds, asserts, fields } => {
      let mut obj = ty::Object::empty();
      for field in fields {
        get(st, ar, field.key);
        let Some(key) = field.key else { continue };
        let ExprData::Prim(Prim::String(s)) = &ar[key] else {
          obj.has_unknown = true;
          continue;
        };
        if obj.known.insert(s.clone(), ty::Ty::ANY).is_some() {
          st.err(key, error::Kind::DuplicateField(s.clone()));
        }
      }
      st.define_self_super();
      define_binds(st, ar, expr, binds, def::ExprDefKindMulti::ObjectLocalBind);
      for field in fields {
        let ty = get(st, ar, field.val);
        let Some(key) = field.key else { continue };
        let ExprData::Prim(Prim::String(s)) = &ar[key] else { continue };
        always!(obj.known.insert(s.clone(), ty).is_some());
      }
      for &cond in asserts {
        get(st, ar, cond);
      }
      st.undefine_self_super();
      for &(lhs, _) in binds {
        undefine(st, ar, lhs);
      }
      st.tys.get(ty::Data::Object(obj))
    }
    ExprData::ObjectComp { name, body, id, ary } => {
      let ary_ty = get(st, ar, *ary);
      must_reachable(st, expr, ary_ty);
      let def = def::ExprDef { expr, kind: def::ExprDefKind::ObjectCompId };
      st.scope.define(*id, ty::Ty::ANY, def.into());
      let name_ty = get(st, ar, *name);
      must_reachable(st, expr, name_ty);
      st.define_self_super();
      let body_ty = get(st, ar, *body);
      must_reachable(st, expr, body_ty);
      undefine(st, ar, *id);
      st.undefine_self_super();
      ty::Ty::OBJECT
    }
    ExprData::Array(exprs) => {
      let mut tys = ty::Union::new();
      for &arg in exprs {
        let ty = get(st, ar, arg);
        tys.insert(ty);
      }
      // we say `[]` has type `set[never]`.
      //
      // having `[]` have type `set[any]` would be "right" as well (since any is the top and bottom
      // type), but if we can avoid any, we should.
      //
      // we could also have it be type `array[never]`, since all sets are arrays, and the empty
      // array is the empty set. but we know it's a set since it's empty - a little bonus.
      let is_set = tys.is_empty();
      let elem = st.tys.get(ty::Data::Union(tys));
      st.tys.get(ty::Data::Array(ty::Array { elem, is_set }))
    }
    ExprData::Subscript { on, idx } => {
      let on_ty = get(st, ar, *on);
      must_reachable(st, expr, on_ty);
      let idx_ty = get(st, ar, *idx);
      must_reachable(st, expr, idx_ty);
      get_subscript(st, ar, on_ty, idx_ty, expr, *on, *idx)
    }
    ExprData::Call { func, positional, named } => {
      let fn_ty = get(st, ar, *func);
      let pos_args: Vec<_> = positional.iter().map(|&arg| (arg, get(st, ar, arg))).collect();
      let mut named_args = FxHashMap::<Id, (Expr, ty::Ty)>::default();
      for &(id, arg) in named {
        let arg_ty = get(st, ar, arg);
        if named_args.insert(id, (arg, arg_ty)).is_some() {
          if let Some(arg) = arg {
            st.err(arg, error::Kind::DuplicateNamedArg(id));
          }
        }
      }
      call::get(st, ar, expr, *func, fn_ty, &pos_args, &named_args)
    }
    ExprData::Id(id) => {
      if let Some((ty, def)) = st.scope.get(*id) {
        st.note_usage(expr, def);
        ty
      } else {
        let suggest = st.str_ar.get_id(*id).and_then(|id| match suggestion::exact(id) {
          Some(x) => Some(x.to_owned()),
          None => suggestion::approx(id, st.scope.all_str(st.str_ar)),
        });
        st.err(expr, error::Kind::UndefinedVar(*id, suggest));
        ty::Ty::ANY
      }
    }
    ExprData::Local { binds, body } => {
      define_binds(st, ar, expr, binds, def::ExprDefKindMulti::LocalBind);
      let ty = get(st, ar, *body);
      for &(bind, _) in binds {
        undefine(st, ar, bind);
      }
      ty
    }
    ExprData::Function { params, body } => {
      let m = def::ExprDefKindMulti::FnParam;
      let param_tys = {
        let mut tmp = FxHashMap::<Id, ty::Ty>::default();
        for (idx, &(id, rhs)) in params.iter().enumerate() {
          if tmp.insert(id, ty::Ty::ANY).is_some() {
            st.err(rhs.flatten().unwrap_or(expr), error::Kind::DuplicateVar(id, idx, m));
          }
        }
        let fs = flow::extract::get_always(&st.scope, ar, *body);
        for (id, fact) in fs.into_iter() {
          if let Some(cur) = tmp.get_mut(&id) {
            always!(*cur == ty::Ty::ANY, "fn param tys should be Any before the facts");
            fact.apply_to(&mut st.tys, cur);
          }
          // ignore facts about non-params.
        }
        tmp
      };
      for (idx, &(id, _)) in params.iter().enumerate() {
        // unwrap_or should never happen, but if it does we'll notice with the always!(false) later
        let ty = param_tys.get(&id).copied().unwrap_or(ty::Ty::ANY);
        let def = def::ExprDef { expr, kind: def::ExprDefKind::Multi(idx, m) };
        st.scope.define(id, ty, def.into());
      }
      for &(id, rhs) in params {
        let Some(rhs) = rhs else { continue };
        let ty = param_tys.get(&id).copied().unwrap_or(ty::Ty::ANY);
        let rhs_ty = get(st, ar, rhs);
        st.unify(rhs.unwrap_or(expr), ty, rhs_ty);
      }
      let body_ty = get(st, ar, *body);
      let mut fn_params = Vec::<ty::Param>::with_capacity(params.len());
      for &(id, rhs) in params {
        undefine(st, ar, id);
        let Some(&ty) = param_tys.get(&id) else {
          always!(false, "should have gotten fn param ty: {id:?}");
          continue;
        };
        fn_params.push(ty::Param { id, ty, required: rhs.is_none() });
      }
      let fn_ty = ty::RegularFn { params: fn_params, ret: body_ty };
      st.tys.get(ty::Data::Fn(ty::Fn::Regular(fn_ty)))
    }
    ExprData::If { cond, yes, no } => {
      let cond_ty = get(st, ar, *cond);
      must_reachable(st, expr, cond_ty);
      st.unify(cond.unwrap_or(expr), ty::Ty::BOOLEAN, cond_ty);
      let mut fs = flow::data::Facts::default();
      flow::extract::get_cond(&st.scope, ar, &mut fs, *cond);
      st.scope.add_facts(&mut st.tys, &fs);
      let yes_ty = get(st, ar, *yes);
      st.scope.remove_facts(&fs);
      fs.negate();
      st.scope.add_facts(&mut st.tys, &fs);
      let no_ty = get(st, ar, *no);
      st.scope.remove_facts(&fs);
      st.tys.get(ty::Data::mk_union([yes_ty, no_ty]))
    }
    ExprData::BinaryOp { lhs, op, rhs } => {
      let lhs_ty = get(st, ar, *lhs);
      let rhs_ty = get(st, ar, *rhs);
      match op {
        BinaryOp::Add => get_add(st, expr, lhs_ty, rhs_ty),
        BinaryOp::Mul
        | BinaryOp::Div
        | BinaryOp::Sub
        | BinaryOp::Shl
        | BinaryOp::Shr
        | BinaryOp::BitXor
        | BinaryOp::BitOr
        | BinaryOp::BitAnd => {
          must_reachable(st, expr, lhs_ty);
          must_reachable(st, expr, rhs_ty);
          st.unify(lhs.unwrap_or(expr), ty::Ty::NUMBER, lhs_ty);
          st.unify(rhs.unwrap_or(expr), ty::Ty::NUMBER, rhs_ty);
          ty::Ty::NUMBER
        }
        BinaryOp::Eq => {
          must_reachable(st, expr, lhs_ty);
          must_reachable(st, expr, rhs_ty);
          ty::Ty::BOOLEAN
        }
        BinaryOp::Lt | BinaryOp::LtEq | BinaryOp::Gt | BinaryOp::GtEq => {
          // TODO something about how the lhs_ty and rhs_ty need to be "similar" somehow (both
          // numbers or both strings, etc)
          must_reachable(st, expr, lhs_ty);
          must_reachable(st, expr, rhs_ty);
          if !is_orderable(st, lhs_ty) {
            st.err(lhs.unwrap_or(expr), error::Kind::Invalid(lhs_ty, error::Invalid::OrdCmp));
          }
          if !is_orderable(st, rhs_ty) {
            st.err(rhs.unwrap_or(expr), error::Kind::Invalid(rhs_ty, error::Invalid::OrdCmp));
          }
          ty::Ty::BOOLEAN
        }
      }
    }
    ExprData::UnaryOp { inner, op } => {
      let inner_ty = get(st, ar, *inner);
      must_reachable(st, expr, inner_ty);
      let e = inner.unwrap_or(expr);
      let want = match op {
        UnaryOp::Neg | UnaryOp::Pos | UnaryOp::BitNot => ty::Ty::NUMBER,
        UnaryOp::LogicalNot => ty::Ty::BOOLEAN,
      };
      st.unify(e, want, inner_ty);
      want
    }
    ExprData::Error(inner) => {
      let inner_ty = get(st, ar, *inner);
      must_reachable(st, expr, inner_ty);
      ty::Ty::NEVER
    }
    ExprData::Import { kind, path } => match kind {
      jsonnet_expr::ImportKind::Code => {
        st.note_usage(expr, def::Def::Import(*path));
        st.import_ty(*path)
      }
      jsonnet_expr::ImportKind::String => ty::Ty::STRING,
      jsonnet_expr::ImportKind::Binary => ty::Ty::ARRAY_NUMBER,
    },
  };
  // NOTE: we CANNOT assert that this always return None. i'm pretty confident it's because of
  // duplication of expressions when lowering array/object comprehensions. i don't think that's a
  // huge problem.
  st.insert_expr_ty(expr, ret);
  ret
}

fn must_reachable(st: &mut st::St<'_>, expr: ExprMust, ty: jsonnet_ty::Ty) {
  let ty::Data::Union(parts) = st.tys.data(ty) else { return };
  if parts.is_empty() {
    st.err(expr, error::Kind::Unreachable);
  }
}

fn get_add(st: &mut st::St<'_>, expr: ExprMust, lhs_ty: ty::Ty, rhs_ty: ty::Ty) -> ty::Ty {
  match (st.tys.data(lhs_ty), st.tys.data(rhs_ty)) {
    (ty::Data::Prim(ty::Prim::Any), _) | (_, ty::Data::Prim(ty::Prim::Any)) => ty::Ty::ANY,
    // add numbers.
    (ty::Data::Prim(ty::Prim::Number), ty::Data::Prim(ty::Prim::Number)) => ty::Ty::NUMBER,
    // if any operand is string, coerce the other to string.
    (ty::Data::Prim(ty::Prim::String), _) | (_, ty::Data::Prim(ty::Prim::String)) => ty::Ty::STRING,
    // concat arrays.
    (ty::Data::Array(lhs), ty::Data::Array(rhs)) => {
      // compute this first for borrow checker
      let both_set = lhs.is_set && rhs.is_set;
      let elem = st.tys.get(ty::Data::mk_union([lhs.elem, rhs.elem]));
      if both_set {
        st.err(expr, error::Kind::AddSets);
      }
      st.tys.get(ty::Data::Array(ty::Array::new(elem)))
    }
    // add object fields.
    (ty::Data::Object(lhs_obj), ty::Data::Object(rhs_obj)) => {
      let mut obj = lhs_obj.clone();
      // right overrides left.
      let rhs_known = rhs_obj.known.iter().map(|(k, &v)| (k.clone(), v));
      obj.known.extend(rhs_known);
      // this has unknown if either has unknown.
      obj.has_unknown = obj.has_unknown || rhs_obj.has_unknown;
      st.tys.get(ty::Data::Object(obj))
    }
    (ty::Data::Union(lhs_tys), _) => {
      let iter = lhs_tys.clone().into_iter().map(|lhs_ty| get_add(st, expr, lhs_ty, rhs_ty));
      let u = ty::Data::Union(iter.collect());
      st.tys.get(u)
    }
    (_, ty::Data::Union(rhs_tys)) => {
      let iter = rhs_tys.clone().into_iter().map(|rhs_ty| get_add(st, expr, lhs_ty, rhs_ty));
      let u = ty::Data::Union(iter.collect());
      st.tys.get(u)
    }
    _ => {
      st.err(expr, error::Kind::Invalid(lhs_ty, error::Invalid::Add(rhs_ty)));
      ty::Ty::ANY
    }
  }
}

#[allow(clippy::single_match_else)]
fn get_subscript(
  st: &mut st::St<'_>,
  ar: &ExprArena,
  on_ty: ty::Ty,
  idx_ty: ty::Ty,
  expr: ExprMust,
  on: Expr,
  idx: Expr,
) -> ty::Ty {
  let idx_expr = idx.unwrap_or(expr);
  match st.tys.data(on_ty).clone() {
    // degenerate case
    ty::Data::Prim(ty::Prim::Any) => ty::Ty::ANY,
    // invalid
    ty::Data::Prim(_) | ty::Data::Fn(_) => {
      st.err(on.unwrap_or(expr), error::Kind::Invalid(on_ty, error::Invalid::Subscript));
      ty::Ty::ANY
    }
    // array indexing
    ty::Data::Array(arr) => {
      st.unify(idx_expr, ty::Ty::NUMBER, idx_ty);
      arr.elem
    }
    // object field get
    ty::Data::Object(obj) => {
      st.unify(idx_expr, ty::Ty::STRING, idx_ty);
      let idx = idx.and_then(|x| match &ar[x] {
        ExprData::Prim(Prim::String(s)) => Some(s),
        _ => None,
      });
      match idx {
        // we do know what field we're asking for.
        Some(s) => {
          match obj.known.get(s) {
            // we know the type of that field.
            Some(&ty) => ty,
            // we don't know the type.
            None => {
              if let Some(u) = error::Unify::no_such_field(st.str_ar, &obj, s) {
                // this would result in a eval-time error if evaluated.
                st.err(idx_expr, error::Kind::Unify(u));
              }
              ty::Ty::ANY
            }
          }
        }
        // we don't know what field we're asking for.
        None => {
          if obj.has_unknown {
            // all bets are off.
            ty::Ty::ANY
          } else {
            // we know it has to be one of the known fields, but we don't know which one.
            st.tys.get(ty::Data::Union(obj.known.values().copied().collect()))
          }
        }
      }
    }
    // recursive
    ty::Data::Union(tys) => {
      let iter = tys.into_iter().map(|on_ty| get_subscript(st, ar, on_ty, idx_ty, expr, on, idx));
      let res = ty::Data::Union(iter.collect());
      st.tys.get(res)
    }
  }
}

fn define_binds(
  st: &mut st::St<'_>,
  ar: &ExprArena,
  expr: jsonnet_expr::ExprMust,
  binds: &[(Id, jsonnet_expr::Expr)],
  m: def::ExprDefKindMulti,
) {
  let mut bound_ids = FxHashSet::<Id>::default();
  for (idx, &(bind, _)) in binds.iter().enumerate() {
    let def = def::ExprDef { expr, kind: def::ExprDefKind::Multi(idx, m) };
    st.scope.define(bind, ty::Ty::ANY, def.into());
    if !bound_ids.insert(bind) {
      st.err(expr, error::Kind::DuplicateVar(bind, idx, m));
    }
  }
  for &(lhs, rhs) in binds {
    let ty = get(st, ar, rhs);
    always!(bound_ids.contains(&lhs), "should have just defined: {lhs:?}");
    st.scope.refine(lhs, ty);
  }
}

fn undefine(st: &mut st::St<'_>, ar: &ExprArena, id: Id) {
  let Some(ed) = st.scope.undefine(id) else { return };
  if is_actually_unused(st, ar, ed) {
    st.err(ed.expr, error::Kind::UnusedVar(id, ed.kind));
  }
}

/// this returns true most of the time. however, in a very specific case, it returns false. that
/// case is when the def is an object comp local bind generated in desugaring that has been seen for
/// the first time.
///
/// if this is its first time, we return false to skip emitting for now. if it is actually used by
/// the second generated occurrence of the id, we will not detect it as unused a second time, and
/// will not ever emit an unused error for it.
///
/// if this is NOT its first time, this is the second and thus final occurrence of the id being
/// detected as unused. so we should actually emit the unused error, so we return true.
///
/// this is pretty hacky, tbh.
fn is_actually_unused(st: &mut st::St<'_>, ar: &ExprArena, expr_def: def::ExprDef) -> bool {
  let def::ExprDef { expr, kind } = expr_def;
  let def::ExprDefKind::Multi(idx, def::ExprDefKindMulti::LocalBind) = kind else { return true };
  let ExprData::Local { binds, body: Some(_) } = &ar[expr] else { return true };
  let Some(&(_, Some(e_idx))) = binds.get(idx) else { return true };
  let ExprData::Subscript { on: Some(on), idx: Some(sub_idx) } = ar[e_idx] else { return true };
  let ExprData::Id(on_id) = ar[on] else { return true };
  if !on_id.is_unutterable() {
    return true;
  }
  let ExprData::Prim(Prim::Number(_)) = ar[sub_idx] else { return true };
  !st.object_comp_local_defs.insert(e_idx)
}

fn is_orderable(st: &st::St<'_>, ty: ty::Ty) -> bool {
  match st.tys.data(ty) {
    ty::Data::Prim(ty::Prim::Any | ty::Prim::Number | ty::Prim::String) => true,
    ty::Data::Prim(ty::Prim::True | ty::Prim::False | ty::Prim::Null)
    | ty::Data::Object(_)
    | ty::Data::Fn(_) => false,
    ty::Data::Array(arr) => is_orderable(st, arr.elem),
    ty::Data::Union(tys) => tys.iter().all(|&ty| is_orderable(st, ty)),
  }
}
