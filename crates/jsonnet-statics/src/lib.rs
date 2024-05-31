//! Static checking for jsonnet.

pub mod error;

use jsonnet_expr::{Arenas, Expr, ExprData, ExprMust, Id, Prim, Str, Subst};
use rustc_hash::{FxHashMap, FxHashSet};

/// A definition site for an identifier.
#[derive(Debug, Clone, Copy)]
pub enum Def {
  /// The standard library, `std`.
  Std,
  /// Keyword identifiers, `self` and `super`.
  KwIdent,
  /// An object comprehension, which are handled somewhat specially.
  ObjectCompId(jsonnet_expr::ExprMust),
  /// A `local` bind.
  LocalBind(jsonnet_expr::ExprMust, usize),
  /// A `function` parameter.
  FunctionParam(jsonnet_expr::ExprMust, usize),
  /// An `import` (Jsonnet code only).
  Import(paths::PathId),
}

impl Def {
  /// Apply a subst.
  pub fn apply(&mut self, subst: &Subst) {
    match self {
      Def::Std
      | Def::KwIdent
      | Def::ObjectCompId(_)
      | Def::LocalBind(_, _)
      | Def::FunctionParam(_, _) => {}
      Def::Import(path_id) => *path_id = subst.get_path_id(*path_id),
    }
  }
}

/// A map from expressions to defs.
pub type DefMap = FxHashMap<jsonnet_expr::ExprMust, Def>;

/// The state when checking statics.
#[derive(Debug, Default)]
pub struct St {
  /// The errors.
  pub errors: Vec<error::Error>,
  /// Any definition sites we could figure out.
  pub defs: DefMap,
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
#[derive(Debug)]
struct Cx {
  /// This is a vec because things go in and out of scope in stacks.
  store: FxHashMap<Id, Vec<Def>>,
}

impl Default for Cx {
  fn default() -> Self {
    let mut ret = Self { store: FxHashMap::default() };
    ret.define(Id::std, Def::Std);
    ret.define(Id::std_unutterable, Def::Std);
    ret
  }
}

impl Cx {
  fn define(&mut self, id: Id, def: Def) {
    self.store.entry(id).or_default().push(def);
  }

  fn undefine(&mut self, id: Id) {
    self.store.entry(id).or_default().pop();
  }

  fn get(&self, id: Id) -> Option<Def> {
    self.store.get(&id)?.last().copied()
  }
}

/// Performs the checks.
pub fn get(st: &mut St, ars: &Arenas, expr: Expr) {
  let mut cx = Cx::default();
  check(st, &mut cx, ars, expr);
}

#[allow(clippy::too_many_lines)]
fn check(st: &mut St, cx: &mut Cx, ars: &Arenas, expr: Expr) {
  let Some(expr) = expr else { return };
  match &ars.expr[expr] {
    ExprData::Prim(_) => {}
    ExprData::Object { asserts, fields } => {
      let mut field_names = FxHashSet::<&Str>::default();
      for field in fields {
        check(st, cx, ars, field.key);
        cx.define(Id::self_, Def::KwIdent);
        cx.define(Id::super_, Def::KwIdent);
        check(st, cx, ars, field.val);
        cx.undefine(Id::self_);
        cx.undefine(Id::super_);
        let Some(key) = field.key else { continue };
        if let ExprData::Prim(Prim::String(s)) = &ars.expr[key] {
          if !field_names.insert(s) {
            st.err(key, error::Kind::DuplicateFieldName(s.clone()));
          }
        }
      }
      cx.define(Id::self_, Def::KwIdent);
      cx.define(Id::super_, Def::KwIdent);
      for &cond in asserts {
        check(st, cx, ars, cond);
      }
    }
    ExprData::ObjectComp { name, body, id, ary } => {
      check(st, cx, ars, *ary);
      cx.define(*id, Def::ObjectCompId(expr));
      check(st, cx, ars, *name);
      cx.define(Id::self_, Def::KwIdent);
      cx.define(Id::super_, Def::KwIdent);
      check(st, cx, ars, *body);
      cx.undefine(*id);
      cx.undefine(Id::self_);
      cx.undefine(Id::super_);
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
      for (idx, &(id, rhs)) in binds.iter().enumerate() {
        cx.define(id, Def::LocalBind(expr, idx));
        if !bound_names.insert(id) {
          st.err(rhs.unwrap_or(expr), error::Kind::DuplicateBinding(id));
        }
      }
      for &(_, rhs) in binds {
        check(st, cx, ars, rhs);
      }
      check(st, cx, ars, *body);
      for &(id, _) in binds {
        cx.undefine(id);
      }
    }
    ExprData::Function { params, body } => {
      let mut bound_names = FxHashSet::<Id>::default();
      for (idx, &(id, rhs)) in params.iter().enumerate() {
        cx.define(id, Def::FunctionParam(expr, idx));
        if !bound_names.insert(id) {
          st.err(rhs.flatten().unwrap_or(expr), error::Kind::DuplicateBinding(id));
        }
      }
      for &(_, rhs) in params {
        let Some(rhs) = rhs else { continue };
        check(st, cx, ars, rhs);
      }
      check(st, cx, ars, *body);
      for &(id, _) in params {
        cx.undefine(id);
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
