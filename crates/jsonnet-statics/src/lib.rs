//! Static checking for jsonnet.

pub mod error;
pub mod ty;

use std::collections::BTreeMap;

use always::always;
use jsonnet_expr::def::{self, Def};
use jsonnet_expr::{Arenas, Expr, ExprData, ExprMust, Id, Prim, Str};
use rustc_hash::{FxHashMap, FxHashSet};

/// The state when checking statics.
#[derive(Debug, Default)]
pub struct St {
  /// The errors.
  pub errors: Vec<error::Error>,
  /// Any definition sites we could figure out.
  pub defs: jsonnet_expr::def::Map,
  /// A store for the types.
  pub tys: ty::Store,
  /// Types of expressions.
  pub expr_tys: ty::Exprs,
}

impl St {
  fn err(&mut self, expr: ExprMust, kind: error::Kind) {
    self.errors.push(error::Error { expr, kind });
  }

  fn note_usage(&mut self, expr: ExprMust, def: Def) {
    // NOTE: we CANNOT assert insert returns none here, because we reuse expr indices sometimes
    // when desugaring.
    self.defs.insert(expr, def);
  }
}

#[derive(Debug)]
struct TrackedDef {
  def: Def,
  usages: usize,
}

/// The context. Stores the identifiers currently in scope.
#[derive(Debug, Default)]
struct Cx {
  /// This is a vec because things go in and out of scope in stacks.
  store: FxHashMap<Id, Vec<TrackedDef>>,
}

impl Cx {
  fn define(&mut self, id: Id, def: Def) {
    self.store.entry(id).or_default().push(TrackedDef { def, usages: 0 });
  }

  fn get(&mut self, id: Id) -> Option<Def> {
    let tracked = self.store.get_mut(&id)?.last_mut()?;
    tracked.usages += 1;
    Some(tracked.def)
  }
}

fn undefine(cx: &mut Cx, st: &mut St, id: Id) {
  let Some(tracked) = cx.store.entry(id).or_default().pop() else {
    always!(false, "undefine without previous define: {id:?}");
    return;
  };
  if tracked.usages != 0 || id == Id::dollar {
    return;
  }
  let Def::Expr(e, k) = tracked.def else { return };
  st.err(e, error::Kind::Unused(id, k));
}

/// Performs the checks.
pub fn get(st: &mut St, ars: &Arenas, expr: Expr) {
  let mut cx = Cx::default();
  cx.define(Id::std, Def::Std);
  cx.define(Id::std_unutterable, Def::Std);
  check(st, &mut cx, ars, expr);
  undefine(&mut cx, st, Id::std);
  undefine(&mut cx, st, Id::std_unutterable);
  for (_, stack) in cx.store {
    always!(stack.is_empty());
  }
}

/// NOTE: don't return early from this except in the degenerate case where the `expr` was `None`.
/// This is so we can insert the expr's type into the `St` at the end.
#[allow(clippy::too_many_lines, clippy::single_match_else)]
fn check(st: &mut St, cx: &mut Cx, ars: &Arenas, expr: Expr) -> ty::Ty {
  let Some(expr) = expr else { return ty::Ty::ANY };
  let ret = match &ars.expr[expr] {
    ExprData::Prim(prim) => st.tys.get(ty::Data::Prim(prim.clone())),
    ExprData::Object { binds, asserts, fields } => {
      let mut field_tys = BTreeMap::<Str, ty::Ty>::default();
      for field in fields {
        check(st, cx, ars, field.key);
        let Some(key) = field.key else { continue };
        let ExprData::Prim(Prim::String(s)) = &ars.expr[key] else { continue };
        if field_tys.insert(s.clone(), ty::Ty::ANY).is_some() {
          st.err(key, error::Kind::DuplicateFieldName(s.clone()));
        }
      }
      cx.define(Id::self_, Def::KwIdent);
      cx.define(Id::super_, Def::KwIdent);
      for (idx, &(lhs, _)) in binds.iter().enumerate() {
        cx.define(lhs, Def::Expr(expr, def::ExprDefKind::ObjectLocal(idx)));
      }
      for &(_, expr) in binds {
        check(st, cx, ars, expr);
      }
      let mut other = false;
      for field in fields {
        let ty = check(st, cx, ars, field.val);
        let Some(key) = field.key else { continue };
        let ExprData::Prim(Prim::String(s)) = &ars.expr[key] else {
          other = true;
          continue;
        };
        always!(field_tys.insert(s.clone(), ty).is_some());
      }
      for &cond in asserts {
        check(st, cx, ars, cond);
      }
      undefine(cx, st, Id::self_);
      undefine(cx, st, Id::super_);
      for &(lhs, _) in binds {
        undefine(cx, st, lhs);
      }
      st.tys.get(ty::Data::Object { known: field_tys, other })
    }
    ExprData::ObjectComp { name, body, id, ary } => {
      check(st, cx, ars, *ary);
      cx.define(*id, Def::Expr(expr, def::ExprDefKind::ObjectCompId));
      check(st, cx, ars, *name);
      cx.define(Id::self_, Def::KwIdent);
      cx.define(Id::super_, Def::KwIdent);
      check(st, cx, ars, *body);
      undefine(cx, st, *id);
      undefine(cx, st, Id::self_);
      undefine(cx, st, Id::super_);
      st.tys.get(ty::Data::Object { known: BTreeMap::new(), other: true })
    }
    ExprData::Array(exprs) => {
      for &arg in exprs {
        check(st, cx, ars, arg);
      }
      st.tys.get(ty::Data::Array(ty::Ty::ANY))
    }
    ExprData::Subscript { on, idx } => {
      check(st, cx, ars, *on);
      check(st, cx, ars, *idx);
      ty::Ty::ANY
    }
    ExprData::Call { func, positional, named } => {
      check(st, cx, ars, *func);
      for &arg in positional {
        check(st, cx, ars, arg);
      }
      let mut arg_names = FxHashSet::<Id>::default();
      for &(id, arg) in named {
        check(st, cx, ars, arg);
        if !arg_names.insert(id) {
          if let Some(arg) = arg {
            st.err(arg, error::Kind::DuplicateNamedArg(id));
          }
        }
      }
      ty::Ty::ANY
    }
    ExprData::Id(id) => match cx.get(*id) {
      Some(def) => {
        st.note_usage(expr, def);
        let is_obj = *id == Id::self_ || *id == Id::super_ || *id == Id::dollar;
        let is_std = *id == Id::std || *id == Id::std_unutterable;
        if is_obj || is_std {
          // TODO do better for std
          ty::Ty::OBJECT
        } else {
          // TODO save types for general ids in cx and use here
          ty::Ty::ANY
        }
      }
      None => {
        st.err(expr, error::Kind::NotInScope(*id));
        ty::Ty::ANY
      }
    },
    ExprData::Local { binds, body } => {
      let mut bound_names = FxHashSet::<Id>::default();
      for (idx, &(bind, rhs)) in binds.iter().enumerate() {
        cx.define(bind, Def::Expr(expr, def::ExprDefKind::LocalBind(idx)));
        if !bound_names.insert(bind) {
          st.err(rhs.unwrap_or(expr), error::Kind::DuplicateBinding(bind));
        }
      }
      for &(_, rhs) in binds {
        check(st, cx, ars, rhs);
      }
      let ty = check(st, cx, ars, *body);
      for &(bind, _) in binds {
        undefine(cx, st, bind);
      }
      ty
    }
    ExprData::Function { params, body } => {
      let mut bound_names = FxHashSet::<Id>::default();
      for (idx, &(bind, rhs)) in params.iter().enumerate() {
        cx.define(bind, Def::Expr(expr, def::ExprDefKind::FnParam(idx)));
        if !bound_names.insert(bind) {
          st.err(rhs.flatten().unwrap_or(expr), error::Kind::DuplicateBinding(bind));
        }
      }
      for &(_, rhs) in params {
        let Some(rhs) = rhs else { continue };
        check(st, cx, ars, rhs);
      }
      check(st, cx, ars, *body);
      for &(bind, _) in params {
        undefine(cx, st, bind);
      }
      let param_tys: Vec<_> = std::iter::repeat(ty::Ty::ANY).take(params.len()).collect();
      st.tys.get(ty::Data::Fn(param_tys, ty::Ty::ANY))
    }
    ExprData::If { cond, yes, no } => {
      check(st, cx, ars, *cond);
      check(st, cx, ars, *yes);
      check(st, cx, ars, *no);
      ty::Ty::ANY
    }
    ExprData::BinaryOp { lhs, rhs, .. } => {
      check(st, cx, ars, *lhs);
      check(st, cx, ars, *rhs);
      ty::Ty::ANY
    }
    ExprData::UnaryOp { inner, .. } | ExprData::Error(inner) => {
      check(st, cx, ars, *inner);
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
  st.expr_tys.insert(expr, ret);
  ret
}
