//! Check expressions for static validity: variables are in scope, types match up, etc.

mod call;

use crate::{error, flow, st, suggestion};
use always::always;
use jsonnet_expr::{BinOp, Expr, ExprArena, ExprData, ExprMust, Id, Prim, UnOp, def};
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
    ExprData::Object { asserts, fields } => {
      let mut obj = ty::Object::empty();
      for field in fields {
        get(st, ar, field.key);
        let Some(key) = field.key else { continue };
        let ExprData::Prim(Prim::String(s)) = &ar[key] else {
          obj.has_unknown = true;
          continue;
        };
        if obj.known.insert(*s, ty::Ty::ANY).is_some() {
          st.err(key, error::Kind::DuplicateField(*s));
        }
      }
      st.define_self_super();
      for field in fields {
        let ty = get(st, ar, field.val);
        let Some(key) = field.key else { continue };
        let ExprData::Prim(Prim::String(s)) = &ar[key] else { continue };
        always!(obj.known.insert(*s, ty).is_some());
      }
      for &cond in asserts {
        get(st, ar, cond);
      }
      st.undefine_self_super();
      st.tys.get(ty::Data::Object(obj))
    }
    ExprData::ObjectComp { name, vis: _, body, id, ary } => {
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
      // we say `[]` has type `array[never]`.
      //
      // having `[]` have type `set[any]` would be "right" as well (since any is the top and bottom
      // type), but if we can avoid any, we should.
      //
      // we could also have it be type `set[never]`, since the empty array is sorted and thus a set.
      // but this makes some errors more annoying.
      let elem = st.tys.get(ty::Data::Union(tys));
      st.tys.get(ty::Data::Array(ty::Array { elem, is_set: false }))
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
    ExprData::Fn { params, body } => {
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
    ExprData::BinOp { lhs, op, rhs } => {
      let lhs_ty = get(st, ar, *lhs);
      let rhs_ty = get(st, ar, *rhs);
      match op {
        BinOp::Add => get_add(st, expr, lhs_ty, rhs_ty),
        BinOp::Mul
        | BinOp::Div
        | BinOp::Sub
        | BinOp::Shl
        | BinOp::Shr
        | BinOp::BitXor
        | BinOp::BitOr
        | BinOp::BitAnd => {
          must_reachable(st, expr, lhs_ty);
          must_reachable(st, expr, rhs_ty);
          st.unify(lhs.unwrap_or(expr), ty::Ty::NUMBER, lhs_ty);
          st.unify(rhs.unwrap_or(expr), ty::Ty::NUMBER, rhs_ty);
          ty::Ty::NUMBER
        }
        BinOp::Eq => {
          must_reachable(st, expr, lhs_ty);
          must_reachable(st, expr, rhs_ty);
          // only err if BOTH are NOT equatable
          if !can_eq(st, lhs_ty) && !can_eq(st, rhs_ty) {
            st.err(expr, error::Kind::Invalid(lhs_ty, error::Invalid::Eq(rhs_ty)));
          }
          ty::Ty::BOOLEAN
        }
        BinOp::Lt | BinOp::LtEq | BinOp::Gt | BinOp::GtEq => {
          must_reachable(st, expr, lhs_ty);
          must_reachable(st, expr, rhs_ty);
          if !can_ord_cmp(&st.tys, lhs_ty, rhs_ty) {
            st.err(expr, error::Kind::Invalid(lhs_ty, error::Invalid::OrdCmp(rhs_ty)));
          }
          ty::Ty::BOOLEAN
        }
      }
    }
    ExprData::UnOp { inner, op } => {
      let inner_ty = get(st, ar, *inner);
      must_reachable(st, expr, inner_ty);
      let e = inner.unwrap_or(expr);
      let want = match op {
        UnOp::Neg | UnOp::Pos | UnOp::BitNot => ty::Ty::NUMBER,
        UnOp::LogicalNot => ty::Ty::BOOLEAN,
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
    ExprData::SubstOuter(e) => get(st, ar, *e),
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
    // put this before the any case - never is it valid to add these things to anything.
    (ty::Data::Fn(_) | ty::Data::Prim(ty::Prim::Null | ty::Prim::True | ty::Prim::False), _)
    | (_, ty::Data::Fn(_) | ty::Data::Prim(ty::Prim::Null | ty::Prim::True | ty::Prim::False)) => {
      st.err(expr, error::Kind::Invalid(lhs_ty, error::Invalid::Add(rhs_ty)));
      ty::Ty::ANY
    }
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
      if rhs_obj.has_unknown {
        obj.has_unknown = true;
        // the unknown field(s) from rhs can override the types of the known field(s) from lhs to
        // anything, but we still know the lhs fields will certainly exist.
        for ty in obj.known.values_mut() {
          *ty = ty::Ty::ANY;
        }
      }
      // right known overrides left known.
      obj.known.extend(rhs_obj.known.iter());
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

fn can_ord_cmp(tys: &ty::MutStore<'_>, lhs: ty::Ty, rhs: ty::Ty) -> bool {
  match (tys.data(lhs), tys.data(rhs)) {
    (ty::Data::Prim(_), ty::Data::Array(_))
    | (ty::Data::Array(_), ty::Data::Prim(_))
    | (ty::Data::Object(_) | ty::Data::Fn(_), _)
    | (_, ty::Data::Object(_) | ty::Data::Fn(_)) => false,
    (ty::Data::Prim(lhs), ty::Data::Prim(rhs)) => match (lhs, rhs) {
      (ty::Prim::True | ty::Prim::False | ty::Prim::Null, _)
      | (_, ty::Prim::True | ty::Prim::False | ty::Prim::Null)
      | (ty::Prim::String, ty::Prim::Number)
      | (ty::Prim::Number, ty::Prim::String) => false,
      (ty::Prim::Number, ty::Prim::Number)
      | (ty::Prim::String, ty::Prim::String)
      | (ty::Prim::Any, ty::Prim::Any | ty::Prim::String | ty::Prim::Number)
      | (ty::Prim::String | ty::Prim::Number, ty::Prim::Any) => true,
    },
    (ty::Data::Array(lhs), ty::Data::Array(rhs)) => can_ord_cmp(tys, lhs.elem, rhs.elem),
    (ty::Data::Union(parts), _) => parts.iter().all(|&lhs| can_ord_cmp(tys, lhs, rhs)),
    (_, ty::Data::Union(parts)) => parts.iter().all(|&rhs| can_ord_cmp(tys, lhs, rhs)),
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
    // string chars (but no char type)
    ty::Data::Prim(ty::Prim::String) => ty::Ty::STRING,
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
      let idx = idx.and_then(|x| match ar[x] {
        ExprData::Prim(Prim::String(s)) => Some(s),
        _ => None,
      });
      match idx {
        // we do know what field we're asking for.
        Some(s) => {
          match obj.known.get(&s) {
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
  if !id.is_unutterable() && is_actually_unused(st, ar, ed) {
    st.err(ed.expr, error::Kind::UnusedVar(id, ed.kind));
  }
}

/// we need to count down all the desugared copies of variables for e.g. object comprehension
/// identifiers
fn is_actually_unused(st: &mut st::St<'_>, ar: &ExprArena, expr_def: def::ExprDef) -> bool {
  let ed = &ar[expr_def.expr];
  let Some(ce) = canonical_expr(ed, expr_def) else {
    log::warn!("expr def {expr_def:?} didn't match up with expr {ed:?}");
    return true;
  };
  st.id_counts.is_done(ce)
}

fn canonical_expr(expr: &jsonnet_expr::ExprData, expr_def: def::ExprDef) -> Expr {
  match expr_def.kind {
    def::ExprDefKind::ObjectCompId => {
      let ExprData::ObjectComp { .. } = expr else { return None };
      Some(expr_def.expr)
    }
    def::ExprDefKind::Multi(n, m) => match m {
      def::ExprDefKindMulti::LocalBind => {
        let ExprData::Local { binds, .. } = expr else { return None };
        let &(_, e) = binds.get(n)?;
        e
      }
      def::ExprDefKindMulti::FnParam => {
        let ExprData::Fn { params, .. } = expr else { return None };
        let &(_, e) = params.get(n)?;
        e.unwrap_or(Some(expr_def.expr))
      }
    },
  }
}

fn can_eq(st: &st::St<'_>, ty: ty::Ty) -> bool {
  match st.tys.data(ty) {
    ty::Data::Prim(_) | ty::Data::Object(_) => true,
    ty::Data::Fn(_) => false,
    ty::Data::Array(arr) => can_eq(st, arr.elem),
    ty::Data::Union(tys) => tys.iter().all(|&ty| can_eq(st, ty)),
  }
}
