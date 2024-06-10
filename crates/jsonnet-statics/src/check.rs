//! Run the checks.

use crate::{error, st, ty};
use always::always;
use jsonnet_expr::def::{self, Def};
use jsonnet_expr::{Arenas, Expr, ExprData, Id, Prim, Str};
use rustc_hash::FxHashMap;
use std::collections::{BTreeMap, BTreeSet};

/// NOTE: don't return early from this except in the degenerate case where the `expr` was `None`.
/// This is so we can insert the expr's type into the `St` at the end.
#[allow(clippy::too_many_lines, clippy::single_match_else, clippy::similar_names)]
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
      let func_ty = get(st, ars, *func);
      let positional_tys: Vec<_> = positional.iter().map(|&arg| get(st, ars, arg)).collect();
      let mut named_tys = FxHashMap::<Id, (Expr, ty::Ty)>::default();
      for &(id, arg) in named {
        let arg_ty = get(st, ars, arg);
        if named_tys.insert(id, (arg, arg_ty)).is_some() {
          if let Some(arg) = arg {
            st.err(arg, error::Kind::DuplicateNamedArg(id));
          }
        }
      }
      if let ty::Data::Fn(fn_data) = st.data(func_ty) {
        let positional_iter = fn_data.params.iter().zip(positional_tys).zip(positional.iter());
        for ((param, ty), arg) in positional_iter {
          st.unify(arg.unwrap_or(expr), param.ty, ty);
        }
        for param in fn_data.params.iter().skip(positional.len()) {
          match named_tys.remove(&param.id) {
            Some((arg, ty)) => st.unify(arg.unwrap_or(expr), param.ty, ty),
            None => {
              if param.required {
                st.err(func.unwrap_or(expr), error::Kind::MissingArgument(param.id, param.ty));
              }
            }
          }
        }
        for (idx, arg) in positional.iter().enumerate().skip(fn_data.params.len()) {
          st.err(arg.unwrap_or(expr), error::Kind::ExtraPositionalArgument(idx + 1));
        }
        for (id, (arg, _)) in named_tys {
          st.err(arg.unwrap_or(expr), error::Kind::ExtraNamedArgument(id));
        }
        fn_data.ret
      } else {
        // TODO: construct an inferred type from the arguments and unify?
        ty::Ty::ANY
      }
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
      let mut param_tys = FxHashMap::<Id, ty::Ty>::default();
      for (idx, &(bind, rhs)) in params.iter().enumerate() {
        let fresh = st.fresh();
        st.define(bind, fresh, Def::Expr(expr, def::ExprDefKind::FnParam(idx)));
        if param_tys.insert(bind, fresh).is_some() {
          st.err(rhs.flatten().unwrap_or(expr), error::Kind::DuplicateBinding(bind));
        }
      }
      for &(_, rhs) in params {
        let Some(rhs) = rhs else { continue };
        get(st, ars, rhs);
      }
      let body_ty = get(st, ars, *body);
      let mut fn_params = Vec::<ty::Param>::with_capacity(params.len());
      for &(id, rhs) in params {
        st.undefine(id);
        let Some(&ty) = param_tys.get(&id) else {
          always!(false, "should have gotten fn param ty: {id:?}");
          continue;
        };
        fn_params.push(ty::Param { id, ty, required: rhs.is_none() });
      }
      let fn_ty = ty::Fn { params: fn_params, ret: body_ty };
      st.get_ty(ty::Data::Fn(fn_ty))
    }
    ExprData::If { cond, yes, no } => {
      let cond_ty = get(st, ars, *cond);
      st.unify(cond.unwrap_or(expr), ty::Ty::BOOL, cond_ty);
      let yes_ty = get(st, ars, *yes);
      let no_ty = get(st, ars, *no);
      st.get_ty(ty::Data::Union(BTreeSet::from_iter([yes_ty, no_ty])))
    }
    ExprData::BinaryOp { lhs, op, rhs } => {
      let lhs_ty = get(st, ars, *lhs);
      let rhs_ty = get(st, ars, *rhs);
      match op {
        jsonnet_expr::BinaryOp::Add => ty::Ty::ANY,
        jsonnet_expr::BinaryOp::Mul
        | jsonnet_expr::BinaryOp::Div
        | jsonnet_expr::BinaryOp::Sub
        | jsonnet_expr::BinaryOp::Shl
        | jsonnet_expr::BinaryOp::Shr
        | jsonnet_expr::BinaryOp::BitXor
        | jsonnet_expr::BinaryOp::BitOr
        | jsonnet_expr::BinaryOp::BitAnd => {
          st.unify(lhs.unwrap_or(expr), ty::Ty::NUMBER, lhs_ty);
          st.unify(rhs.unwrap_or(expr), ty::Ty::NUMBER, rhs_ty);
          ty::Ty::NUMBER
        }
        jsonnet_expr::BinaryOp::Eq => ty::Ty::BOOL,
        jsonnet_expr::BinaryOp::Lt
        | jsonnet_expr::BinaryOp::LtEq
        | jsonnet_expr::BinaryOp::Gt
        | jsonnet_expr::BinaryOp::GtEq => {
          // TODO something about how the lhs_ty and rhs_ty need to be "similar" somehow (both
          // numbers or both strings, etc)
          if !is_comparable(st, lhs_ty) {
            st.err(lhs.unwrap_or(expr), error::Kind::Incomparable(lhs_ty));
          }
          if !is_comparable(st, rhs_ty) {
            st.err(rhs.unwrap_or(expr), error::Kind::Incomparable(rhs_ty));
          }
          ty::Ty::BOOL
        }
      }
    }
    ExprData::UnaryOp { inner, op } => {
      let inner_ty = get(st, ars, *inner);
      let e = inner.unwrap_or(expr);
      let want = match op {
        jsonnet_expr::UnaryOp::Neg | jsonnet_expr::UnaryOp::Pos | jsonnet_expr::UnaryOp::BitNot => {
          ty::Ty::NUMBER
        }
        jsonnet_expr::UnaryOp::LogicalNot => ty::Ty::BOOL,
      };
      st.unify(e, want, inner_ty);
      want
    }
    ExprData::Error(inner) => {
      get(st, ars, *inner);
      ty::Ty::NEVER
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

fn is_comparable(st: &st::St<'_>, ty: ty::Ty) -> bool {
  match st.data(ty) {
    // TODO for the Meta case, should note that the meta var must be comparable. failing to do so is
    // unsound. (but we already have Any so *shrug*)
    ty::Data::Any
    | ty::Data::Meta(_)
    | ty::Data::String
    | ty::Data::Number
    | ty::Data::Prim(Prim::String(_) | Prim::Number(_)) => true,
    ty::Data::Array(ty) => is_comparable(st, ty),
    ty::Data::Union(tys) => tys.iter().any(|&ty| is_comparable(st, ty)),
    ty::Data::Bool
    | ty::Data::Prim(Prim::Null | Prim::Bool(_))
    | ty::Data::Object(_)
    | ty::Data::Fn(_) => false,
  }
}
