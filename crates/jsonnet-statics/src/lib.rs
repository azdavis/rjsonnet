//! Static checking for jsonnet.

pub mod error;

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

#[allow(clippy::too_many_lines)]
fn check(st: &mut St, cx: &mut Cx, ars: &Arenas, expr: Expr) {
  let Some(expr) = expr else { return };
  match &ars.expr[expr] {
    ExprData::Prim(_) => {}
    ExprData::Object { binds, asserts, fields } => {
      let mut field_names = FxHashSet::<&Str>::default();
      for field in fields {
        check(st, cx, ars, field.key);
        let Some(key) = field.key else { continue };
        if let ExprData::Prim(Prim::String(s)) = &ars.expr[key] {
          if !field_names.insert(s) {
            st.err(key, error::Kind::DuplicateFieldName(s.clone()));
          }
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
      for field in fields {
        check(st, cx, ars, field.val);
      }
      for &cond in asserts {
        check(st, cx, ars, cond);
      }
      undefine(cx, st, Id::self_);
      undefine(cx, st, Id::super_);
      for &(lhs, _) in binds {
        undefine(cx, st, lhs);
      }
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
    }
    ExprData::Array(exprs) => {
      for &arg in exprs {
        check(st, cx, ars, arg);
      }
    }
    ExprData::Subscript { on, idx } => {
      check(st, cx, ars, *on);
      check(st, cx, ars, *idx);
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
    }
    ExprData::Id(id) => match cx.get(*id) {
      Some(def) => st.note_usage(expr, def),
      None => st.err(expr, error::Kind::NotInScope(*id)),
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
      check(st, cx, ars, *body);
      for &(bind, _) in binds {
        undefine(cx, st, bind);
      }
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
    }
    ExprData::If { cond, yes, no } => {
      check(st, cx, ars, *cond);
      check(st, cx, ars, *yes);
      check(st, cx, ars, *no);
    }
    ExprData::BinaryOp { lhs, rhs, .. } => {
      check(st, cx, ars, *lhs);
      check(st, cx, ars, *rhs);
    }
    ExprData::UnaryOp { inner, .. } | ExprData::Error(inner) => {
      check(st, cx, ars, *inner);
    }
    ExprData::Import { kind, path } => match kind {
      jsonnet_expr::ImportKind::Code => st.note_usage(expr, Def::Import(*path)),
      jsonnet_expr::ImportKind::String | jsonnet_expr::ImportKind::Binary => {}
    },
  }
}
