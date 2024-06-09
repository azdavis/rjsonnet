//! Run the checks.

use crate::{error, st, ty};
use always::always;
use jsonnet_expr::def::{self, Def};
use jsonnet_expr::{Arenas, Expr, ExprData, Id, Prim, Str};
use rustc_hash::{FxHashMap, FxHashSet};
use std::collections::{BTreeMap, BTreeSet};

/// NOTE: don't return early from this except in the degenerate case where the `expr` was `None`.
/// This is so we can insert the expr's type into the `St` at the end.
#[allow(clippy::too_many_lines, clippy::single_match_else)]
pub(crate) fn get(st: &mut st::St<'_>, ars: &Arenas, expr: Expr) -> ty::Ty {
  let Some(expr) = expr else { return ty::Ty::ANY };
  let ret = match &ars.expr[expr] {
    ExprData::Prim(prim) => st.get_ty(ty::Data::Prim(prim.clone())),
    ExprData::Object { binds, asserts, fields } => {
      let mut field_tys = BTreeMap::<Str, ty::Ty>::default();
      for field in fields {
        get(st, ars, field.key);
        let Some(key) = field.key else { continue };
        let ExprData::Prim(Prim::String(s)) = &ars.expr[key] else { continue };
        let fresh = st.fresh();
        if field_tys.insert(s.clone(), fresh).is_some() {
          st.err(key, error::Kind::DuplicateFieldName(s.clone()));
        }
      }
      st.define_self_super();
      let mut bind_tys = FxHashMap::<Id, ty::Ty>::default();
      for (idx, &(lhs, rhs)) in binds.iter().enumerate() {
        let fresh = st.fresh();
        st.define(lhs, fresh, Def::Expr(expr, def::ExprDefKind::ObjectLocal(idx)));
        if bind_tys.insert(lhs, fresh).is_some() {
          st.err(rhs.unwrap_or(expr), error::Kind::DuplicateBinding(lhs));
        }
      }
      for &(lhs, rhs) in binds {
        let ty = get(st, ars, rhs);
        match bind_tys.get(&lhs) {
          None => {
            always!(false, "just defined this name: {lhs:?}");
          }
          Some(&t) => st.unify(rhs.unwrap_or(expr), t, ty),
        }
      }
      for field in fields {
        let ty = get(st, ars, field.val);
        let Some(key) = field.key else { continue };
        let ExprData::Prim(Prim::String(s)) = &ars.expr[key] else {
          // TODO handle when this has other fields
          continue;
        };
        always!(field_tys.insert(s.clone(), ty).is_some());
      }
      for &cond in asserts {
        get(st, ars, cond);
      }
      st.undefine_self_super();
      for &(lhs, _) in binds {
        st.undefine(lhs);
      }
      st.get_ty(ty::Data::Object(field_tys))
    }
    ExprData::ObjectComp { name, body, id, ary } => {
      get(st, ars, *ary);
      st.define(*id, ty::Ty::ANY, Def::Expr(expr, def::ExprDefKind::ObjectCompId));
      get(st, ars, *name);
      st.define_self_super();
      get(st, ars, *body);
      st.undefine(*id);
      st.undefine_self_super();
      // TODO how to model an object with all fields unknown?
      ty::Ty::ANY
    }
    ExprData::Array(exprs) => {
      let mut tys = BTreeSet::<ty::Ty>::new();
      for &arg in exprs {
        let ty = get(st, ars, arg);
        tys.insert(ty);
      }
      let elem_ty = st.get_ty(ty::Data::Union(tys));
      st.get_ty(ty::Data::Array(elem_ty))
    }
    ExprData::Subscript { on, idx } => {
      let on_ty = get(st, ars, *on);
      let idx_ty = get(st, ars, *idx);
      let idx_expr = idx.unwrap_or(expr);
      match st.data(on_ty) {
        ty::Data::Array(elem_ty) => {
          st.unify(idx_expr, ty::Ty::NUMBER, idx_ty);
          elem_ty
        }
        ty::Data::Object(fields) => {
          st.unify(idx_expr, ty::Ty::STRING, idx_ty);
          let idx = idx.and_then(|x| match &ars.expr[x] {
            ExprData::Prim(Prim::String(s)) => Some(s),
            _ => None,
          });
          match idx {
            Some(s) => match fields.get(s) {
              Some(&ty) => ty,
              None => {
                st.err(idx_expr, error::Kind::MissingField(s.clone()));
                ty::Ty::ANY
              }
            },
            None => st.get_ty(ty::Data::Union(fields.values().copied().collect())),
          }
        }
        _ => {
          st.unify(on.unwrap_or(expr), ty::Ty::ARRAY_OR_OBJECT, on_ty);
          ty::Ty::ANY
        }
      }
    }
    ExprData::Call { func, positional, named } => {
      get(st, ars, *func);
      for &arg in positional {
        get(st, ars, arg);
      }
      let mut arg_names = FxHashSet::<Id>::default();
      for &(id, arg) in named {
        get(st, ars, arg);
        if !arg_names.insert(id) {
          if let Some(arg) = arg {
            st.err(arg, error::Kind::DuplicateNamedArg(id));
          }
        }
      }
      ty::Ty::ANY
    }
    ExprData::Id(id) => match st.get(*id) {
      Some((ty, def)) => {
        st.note_usage(expr, def);
        ty
      }
      None => {
        st.err(expr, error::Kind::NotInScope(*id));
        ty::Ty::ANY
      }
    },
    ExprData::Local { binds, body } => {
      let mut bind_tys = FxHashMap::<Id, ty::Ty>::default();
      for (idx, &(bind, rhs)) in binds.iter().enumerate() {
        let fresh = st.fresh();
        st.define(bind, fresh, Def::Expr(expr, def::ExprDefKind::LocalBind(idx)));
        if bind_tys.insert(bind, fresh).is_some() {
          st.err(rhs.unwrap_or(expr), error::Kind::DuplicateBinding(bind));
        }
      }
      for &(lhs, rhs) in binds {
        let ty = get(st, ars, rhs);
        match bind_tys.get(&lhs) {
          None => {
            always!(false, "just defined this name: {lhs:?}");
          }
          Some(&t) => st.unify(rhs.unwrap_or(expr), t, ty),
        }
      }
      let ty = get(st, ars, *body);
      for &(bind, _) in binds {
        st.undefine(bind);
      }
      ty
    }
    ExprData::Function { params, body } => {
      let mut param_tys = FxHashMap::<Id, (ty::Ty, bool)>::default();
      for (idx, &(bind, rhs)) in params.iter().enumerate() {
        let fresh = st.fresh();
        st.define(bind, fresh, Def::Expr(expr, def::ExprDefKind::FnParam(idx)));
        if param_tys.insert(bind, (fresh, rhs.is_none())).is_some() {
          st.err(rhs.flatten().unwrap_or(expr), error::Kind::DuplicateBinding(bind));
        }
      }
      for &(_, rhs) in params {
        let Some(rhs) = rhs else { continue };
        get(st, ars, rhs);
      }
      let body_ty = get(st, ars, *body);
      for &(bind, _) in params {
        st.undefine(bind);
      }
      let fn_ty = ty::Fn {
        params: param_tys
          .iter()
          .map(|(&id, &(ty, required))| ty::Param { id, ty, required })
          .collect(),
        ret: body_ty,
      };
      st.get_ty(ty::Data::Fn(fn_ty))
    }
    ExprData::If { cond, yes, no } => {
      let cond_ty = get(st, ars, *cond);
      st.unify(cond.unwrap_or(expr), ty::Ty::BOOL, cond_ty);
      let yes_ty = get(st, ars, *yes);
      let no_ty = get(st, ars, *no);
      st.get_ty(ty::Data::Union(BTreeSet::from_iter([yes_ty, no_ty])))
    }
    ExprData::BinaryOp { lhs, rhs, .. } => {
      get(st, ars, *lhs);
      get(st, ars, *rhs);
      ty::Ty::ANY
    }
    ExprData::UnaryOp { inner, .. } | ExprData::Error(inner) => {
      get(st, ars, *inner);
      ty::Ty::ANY
    }
    ExprData::Import { kind, path } => match kind {
      jsonnet_expr::ImportKind::Code => {
        st.note_usage(expr, Def::Import(*path));
        ty::Ty::ANY
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
