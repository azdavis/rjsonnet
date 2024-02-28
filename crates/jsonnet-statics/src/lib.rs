//! Static checking for jsonnet.

pub mod error;

use jsonnet_expr::{Arenas, Expr, ExprData, ExprMust, Id, Prim, Str};
use rustc_hash::{FxHashMap, FxHashSet};

/// A definition site for an identifier.
#[derive(Debug, Clone, Copy)]
pub enum Def {
  /// Things like std, self, and super.
  Builtin,
  /// An object comprehension, which are handled somewhat specially.
  ObjectCompId(jsonnet_expr::ExprMust),
  /// A `local` bind.
  LocalBind(jsonnet_expr::ExprMust, usize),
  /// A `function` parameter.
  FunctionParam(jsonnet_expr::ExprMust, usize),
  /// An `import` (Jsonnet code only).
  Import(paths::PathId),
}

/// The state when checking statics.
#[derive(Debug, Default)]
pub struct St {
  /// The errors.
  pub errors: Vec<error::Error>,
  /// Any definition sites we could figure out.
  pub defs: FxHashMap<jsonnet_expr::ExprMust, Def>,
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

/// The context. Stores the identifiers currently in scope.
#[derive(Debug, Clone)]
struct Cx {
  store: FxHashMap<Id, Def>,
}

impl Default for Cx {
  fn default() -> Self {
    let mut ret = Self { store: FxHashMap::default() };
    ret.define(Id::std, Def::Builtin);
    ret.define(Id::std_unutterable, Def::Builtin);
    ret
  }
}

impl Cx {
  fn define(&mut self, id: Id, def: Def) {
    self.store.insert(id, def);
  }

  fn get(&self, id: Id) -> Option<Def> {
    self.store.get(&id).copied()
  }
}

/// Performs the checks.
pub fn get(st: &mut St, ars: &Arenas, expr: Expr) {
  let cx = Cx::default();
  check(st, &cx, ars, expr);
}

#[allow(clippy::too_many_lines)]
fn check(st: &mut St, cx: &Cx, ars: &Arenas, expr: Expr) {
  let Some(expr) = expr else { return };
  match &ars.expr[expr] {
    ExprData::Prim(_) => {}
    ExprData::Object { asserts, fields } => {
      let cx_big = {
        let mut cx = cx.clone();
        cx.define(Id::self_, Def::Builtin);
        cx.define(Id::super_, Def::Builtin);
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
      cx.define(*id, Def::ObjectCompId(expr));
      check(st, &cx, ars, *name);
      cx.define(Id::self_, Def::Builtin);
      cx.define(Id::super_, Def::Builtin);
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
    ExprData::Id(id) => match cx.get(*id) {
      Some(def) => st.note_usage(expr, def),
      None => st.err(expr, error::Kind::NotInScope(*id)),
    },
    ExprData::Local { binds, body } => {
      let mut cx = cx.clone();
      let mut bound_names = FxHashSet::<Id>::default();
      for (idx, &(id, rhs)) in binds.iter().enumerate() {
        cx.define(id, Def::LocalBind(expr, idx));
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
      for (idx, &(id, rhs)) in params.iter().enumerate() {
        cx.define(id, Def::FunctionParam(expr, idx));
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
    ExprData::Import { kind, path } => match kind {
      jsonnet_expr::ImportKind::Code => st.note_usage(expr, Def::Import(*path)),
      jsonnet_expr::ImportKind::String | jsonnet_expr::ImportKind::Binary => {}
    },
  }
}
