//! Static checking for jsonnet.

pub mod error;

use jsonnet_expr::{Arenas, Expr, ExprData, ExprMust, Id, Prim, Str};
use rustc_hash::FxHashSet;

/// The state when checking statics.
#[derive(Debug, Default)]
pub struct St {
  errors: Vec<error::Error>,
}

impl St {
  fn err(&mut self, expr: ExprMust, kind: error::Kind) {
    self.errors.push(error::Error { expr, kind });
  }

  /// Returns all the errors accumulated in the state.
  #[must_use]
  pub fn finish(self) -> Vec<error::Error> {
    self.errors
  }
}

/// The context. Stores the identifiers currently in scope.
#[derive(Debug, Clone)]
pub struct Cx {
  store: FxHashSet<Id>,
}

impl Default for Cx {
  fn default() -> Self {
    let mut ret = Self { store: FxHashSet::default() };
    ret.insert(Id::std);
    ret.insert(Id::std_unutterable);
    ret
  }
}

impl Cx {
  fn insert(&mut self, id: Id) {
    self.store.insert(id);
  }

  fn contains(&self, id: Id) -> bool {
    self.store.contains(&id)
  }
}

/// Performs the checks.
#[allow(clippy::too_many_lines)]
pub fn check(st: &mut St, cx: &Cx, ars: &Arenas, expr: Expr) {
  let Some(expr) = expr else { return };
  match &ars.expr[expr] {
    ExprData::Prim(_) | ExprData::Import { .. } => {}
    ExprData::Object { asserts, fields } => {
      let cx_big = {
        let mut cx = cx.clone();
        cx.insert(Id::self_);
        cx.insert(Id::super_);
        cx
      };
      let mut field_names = FxHashSet::<&Str>::default();
      for &(name, _, body) in fields {
        check(st, cx, ars, name);
        check(st, &cx_big, ars, body);
        let Some(name) = name else { continue };
        if let ExprData::Prim(Prim::String(s)) = &ars.expr[name] {
          if !field_names.insert(s) {
            st.err(name, error::Kind::DuplicateFieldName(s.clone()));
          }
        }
      }
      for &cond in asserts {
        check(st, &cx_big, ars, cond);
      }
    }
    ExprData::ObjectComp { name, body, id, ary } => {
      check(st, cx, ars, *ary);
      let mut cx = cx.clone();
      cx.insert(*id);
      check(st, &cx, ars, *name);
      cx.insert(Id::self_);
      cx.insert(Id::super_);
      check(st, &cx, ars, *body);
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
    ExprData::Id(id) => {
      if !cx.contains(*id) {
        st.err(expr, error::Kind::NotInScope(*id));
      }
    }
    ExprData::Local { binds, body } => {
      let mut cx = cx.clone();
      let mut bound_names = FxHashSet::<Id>::default();
      for &(id, rhs) in binds {
        cx.insert(id);
        if !bound_names.insert(id) {
          st.err(rhs.unwrap_or(expr), error::Kind::DuplicateBinding(id));
        }
      }
      for &(_, rhs) in binds {
        check(st, &cx, ars, rhs);
      }
      check(st, &cx, ars, *body);
    }
    ExprData::Function { params, body } => {
      let mut cx = cx.clone();
      let mut bound_names = FxHashSet::<Id>::default();
      for &(id, rhs) in params {
        cx.insert(id);
        if !bound_names.insert(id) {
          st.err(rhs.flatten().unwrap_or(expr), error::Kind::DuplicateBinding(id));
        }
      }
      for &(_, rhs) in params {
        let Some(rhs) = rhs else { continue };
        check(st, &cx, ars, rhs);
      }
      check(st, &cx, ars, *body);
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
  }
}
