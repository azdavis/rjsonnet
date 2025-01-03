//! Check expressions for static validity: variables are in scope, types match up, etc.

mod call;

use crate::{error, facts, scope, st, suggestion};
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
        st.undefine(lhs);
      }
      st.tys.get(ty::Data::Object(obj))
    }
    ExprData::ObjectComp { name, body, id, ary } => {
      get(st, ar, *ary);
      st.scope.define(*id, ty::Ty::ANY, def::Def::Expr(expr, def::ExprDefKind::ObjectCompId));
      get(st, ar, *name);
      st.define_self_super();
      get(st, ar, *body);
      st.undefine(*id);
      st.undefine_self_super();
      ty::Ty::OBJECT
    }
    ExprData::Array(exprs) => {
      let mut tys = ty::Union::new();
      for &arg in exprs {
        let ty = get(st, ar, arg);
        tys.insert(ty);
      }
      // we say `[]` has type `any[]`.
      //
      // having `[]` have type `never[]` would technically be right, but causes some strangeness,
      // e.g. with the special-case typing of `std.join`. i suppose it might also be a bit jarring
      // for users to see the `never`, which rarely comes up in user code.
      //
      // we could also have it be type `set[any]`, since all sets are arrays, and the empty array is
      // the empty set. but using `[]` as an empty array seems more common. if a user needs the
      // empty set explicitly, they can use `std.set([])`.
      let elem = if tys.is_empty() { ty::Ty::ANY } else { st.tys.get(ty::Data::Union(tys)) };
      st.tys.get(ty::Data::Array(ty::Array::new(elem)))
    }
    ExprData::Subscript { on, idx } => {
      let on_ty = get(st, ar, *on);
      let idx_ty = get(st, ar, *idx);
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
      call::get(st, expr, *func, fn_ty, &pos_args, &named_args)
    }
    ExprData::Id(id) => {
      if let Some((ty, def)) = st.scope.get(*id) {
        st.note_usage(expr, def);
        ty
      } else {
        let suggest = match suggestion::exact(st.str_ar.get_id(*id)) {
          Some(x) => Some(x.to_owned()),
          None => suggestion::approx(st.str_ar.get_id(*id), st.scope.all_str(st.str_ar)),
        };
        st.err(expr, error::Kind::UndefinedVar(*id, suggest));
        ty::Ty::ANY
      }
    }
    ExprData::Local { binds, body } => {
      define_binds(st, ar, expr, binds, def::ExprDefKindMulti::LocalBind);
      let ty = get(st, ar, *body);
      for &(bind, _) in binds {
        st.undefine(bind);
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
        let fs = facts::get_always(&mut st.tys, &st.scope, ar, *body);
        for (id, fact) in fs.into_iter() {
          if let Some(cur) = tmp.get_mut(&id) {
            *cur = fact.into_ty(&mut st.tys);
          }
          // ignore facts about non-params.
        }
        tmp
      };
      for (idx, &(id, _)) in params.iter().enumerate() {
        // unwrap_or should never happen, but if it does we'll notice with the always!(false) later
        let ty = param_tys.get(&id).copied().unwrap_or(ty::Ty::ANY);
        st.scope.define(id, ty, def::Def::Expr(expr, def::ExprDefKind::Multi(idx, m)));
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
        st.undefine(id);
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
      st.unify(cond.unwrap_or(expr), ty::Ty::BOOL, cond_ty);
      let mut fs = scope::Facts::default();
      facts::get_cond(&mut st.tys, &st.scope, ar, &mut fs, *cond);
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
          st.unify(lhs.unwrap_or(expr), ty::Ty::NUMBER, lhs_ty);
          st.unify(rhs.unwrap_or(expr), ty::Ty::NUMBER, rhs_ty);
          ty::Ty::NUMBER
        }
        BinaryOp::Eq => {
          st.unify(rhs.unwrap_or(expr), lhs_ty, rhs_ty);
          ty::Ty::BOOL
        }
        BinaryOp::Lt | BinaryOp::LtEq | BinaryOp::Gt | BinaryOp::GtEq => {
          // TODO something about how the lhs_ty and rhs_ty need to be "similar" somehow (both
          // numbers or both strings, etc)
          if !is_orderable(st, lhs_ty) {
            st.err(lhs.unwrap_or(expr), error::Kind::Invalid(lhs_ty, error::Invalid::OrdCmp));
          }
          if !is_orderable(st, rhs_ty) {
            st.err(rhs.unwrap_or(expr), error::Kind::Invalid(rhs_ty, error::Invalid::OrdCmp));
          }
          ty::Ty::BOOL
        }
      }
    }
    ExprData::UnaryOp { inner, op } => {
      let inner_ty = get(st, ar, *inner);
      let e = inner.unwrap_or(expr);
      let want = match op {
        UnaryOp::Neg | UnaryOp::Pos | UnaryOp::BitNot => ty::Ty::NUMBER,
        UnaryOp::LogicalNot => ty::Ty::BOOL,
      };
      st.unify(e, want, inner_ty);
      want
    }
    ExprData::Error(inner) => {
      get(st, ar, *inner);
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
    st.scope.define(bind, ty::Ty::ANY, def::Def::Expr(expr, def::ExprDefKind::Multi(idx, m)));
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
