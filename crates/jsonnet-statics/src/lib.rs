//! Static checking for jsonnet.

mod unify;

pub mod error;
pub mod st;
pub mod ty;

use always::always;
use jsonnet_expr::def::{self, Def};
use jsonnet_expr::{Arenas, Expr, ExprData, Id, Prim, Str};
use rustc_hash::FxHashSet;
use std::collections::BTreeMap;

/// Performs the checks.
#[must_use]
pub fn get(mut st: st::St<'_>, ars: &Arenas, expr: Expr) -> st::Statics {
  st.define(Id::std, ty::Ty::ANY, Def::Std);
  st.define(Id::std_unutterable, ty::Ty::ANY, Def::Std);
  check(&mut st, ars, expr);
  st.undefine(Id::std);
  st.undefine(Id::std_unutterable);
  st.finish()
}

/// NOTE: don't return early from this except in the degenerate case where the `expr` was `None`.
/// This is so we can insert the expr's type into the `St` at the end.
#[allow(clippy::too_many_lines, clippy::single_match_else)]
fn check(st: &mut st::St<'_>, ars: &Arenas, expr: Expr) -> ty::Ty {
  let Some(expr) = expr else { return ty::Ty::ANY };
  let ret = match &ars.expr[expr] {
    ExprData::Prim(prim) => st.get_ty(ty::Data::Prim(prim.clone())),
    ExprData::Object { binds, asserts, fields } => {
      let mut field_tys = BTreeMap::<Str, ty::Ty>::default();
      for field in fields {
        check(st, ars, field.key);
        let Some(key) = field.key else { continue };
        let ExprData::Prim(Prim::String(s)) = &ars.expr[key] else { continue };
        if field_tys.insert(s.clone(), ty::Ty::ANY).is_some() {
          st.err(key, error::Kind::DuplicateFieldName(s.clone()));
        }
      }
      st.define_self_super();
      for (idx, &(lhs, _)) in binds.iter().enumerate() {
        st.define(lhs, ty::Ty::ANY, Def::Expr(expr, def::ExprDefKind::ObjectLocal(idx)));
      }
      for &(_, expr) in binds {
        check(st, ars, expr);
      }
      for field in fields {
        let ty = check(st, ars, field.val);
        let Some(key) = field.key else { continue };
        let ExprData::Prim(Prim::String(s)) = &ars.expr[key] else {
          // TODO handle when this has other fields
          continue;
        };
        always!(field_tys.insert(s.clone(), ty).is_some());
      }
      for &cond in asserts {
        check(st, ars, cond);
      }
      st.undefine_self_super();
      for &(lhs, _) in binds {
        st.undefine(lhs);
      }
      st.get_ty(ty::Data::Object(field_tys))
    }
    ExprData::ObjectComp { name, body, id, ary } => {
      check(st, ars, *ary);
      st.define(*id, ty::Ty::ANY, Def::Expr(expr, def::ExprDefKind::ObjectCompId));
      check(st, ars, *name);
      st.define_self_super();
      check(st, ars, *body);
      st.undefine(*id);
      st.undefine_self_super();
      ty::Ty::ANY
    }
    ExprData::Array(exprs) => {
      for &arg in exprs {
        check(st, ars, arg);
      }
      st.get_ty(ty::Data::Array(ty::Ty::ANY))
    }
    ExprData::Subscript { on, idx } => {
      check(st, ars, *on);
      check(st, ars, *idx);
      ty::Ty::ANY
    }
    ExprData::Call { func, positional, named } => {
      check(st, ars, *func);
      for &arg in positional {
        check(st, ars, arg);
      }
      let mut arg_names = FxHashSet::<Id>::default();
      for &(id, arg) in named {
        check(st, ars, arg);
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
      let mut bound_names = FxHashSet::<Id>::default();
      for (idx, &(bind, rhs)) in binds.iter().enumerate() {
        st.define(bind, ty::Ty::ANY, Def::Expr(expr, def::ExprDefKind::LocalBind(idx)));
        if !bound_names.insert(bind) {
          st.err(rhs.unwrap_or(expr), error::Kind::DuplicateBinding(bind));
        }
      }
      for &(_, rhs) in binds {
        check(st, ars, rhs);
      }
      let ty = check(st, ars, *body);
      for &(bind, _) in binds {
        st.undefine(bind);
      }
      ty
    }
    ExprData::Function { params, body } => {
      let mut bound_names = FxHashSet::<Id>::default();
      for (idx, &(bind, rhs)) in params.iter().enumerate() {
        st.define(bind, ty::Ty::ANY, Def::Expr(expr, def::ExprDefKind::FnParam(idx)));
        if !bound_names.insert(bind) {
          st.err(rhs.flatten().unwrap_or(expr), error::Kind::DuplicateBinding(bind));
        }
      }
      for &(_, rhs) in params {
        let Some(rhs) = rhs else { continue };
        check(st, ars, rhs);
      }
      check(st, ars, *body);
      for &(bind, _) in params {
        st.undefine(bind);
      }
      let fn_ty = ty::Fn {
        params: params
          .iter()
          .map(|&(id, default)| ty::Param { id, ty: ty::Ty::ANY, required: default.is_none() })
          .collect(),
        ret: ty::Ty::ANY,
      };
      st.get_ty(ty::Data::Fn(fn_ty))
    }
    ExprData::If { cond, yes, no } => {
      check(st, ars, *cond);
      check(st, ars, *yes);
      check(st, ars, *no);
      ty::Ty::ANY
    }
    ExprData::BinaryOp { lhs, rhs, .. } => {
      check(st, ars, *lhs);
      check(st, ars, *rhs);
      ty::Ty::ANY
    }
    ExprData::UnaryOp { inner, .. } | ExprData::Error(inner) => {
      check(st, ars, *inner);
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
