//! Static checking for jsonnet.

pub mod error;

use always::always;
use jsonnet_expr::def::{self, Def};
use jsonnet_expr::{Arenas, Expr, ExprData, ExprMust, Id, Prim, Str};
use rustc_hash::{FxHashMap, FxHashSet};
use std::collections::hash_map::Entry;

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
struct Usage {
  id: Id,
  count: usize,
}

/// The context. Stores the identifiers currently in scope.
#[derive(Debug, Default)]
struct Cx {
  /// This is a vec because things go in and out of scope in stacks.
  store: FxHashMap<Id, Vec<Def>>,
  plain_usage: FxHashMap<(ExprMust, def::Plain), Usage>,
  sugary_usage: FxHashMap<def::Sugary, (Usage, ExprMust, def::Plain)>,
}

impl Cx {
  fn define(&mut self, id: Id, def: Def) {
    self.store.entry(id).or_default().push(def);
    let Def::Expr(w) = def else { return };
    always!(id != Id::std);
    always!(id != Id::std_unutterable);
    always!(id != Id::self_);
    always!(id != Id::super_);
    match w.sugary {
      Some(sugary) => match self.sugary_usage.entry(sugary) {
        Entry::Occupied(entry) => {
          always!(entry.get().0.id == id);
        }
        Entry::Vacant(entry) => {
          entry.insert((Usage { id, count: 0 }, w.expr, w.plain));
        }
      },
      None => match self.plain_usage.entry((w.expr, w.plain)) {
        Entry::Occupied(entry) => {
          always!(entry.get().id == id);
        }
        Entry::Vacant(entry) => {
          entry.insert(Usage { id, count: 0 });
        }
      },
    }
  }

  fn get(&mut self, id: Id) -> Option<Def> {
    let &def = self.store.get(&id)?.last()?;
    if let Def::Expr(w) = def {
      let usage = match w.sugary {
        Some(sugary) => self.sugary_usage.get_mut(&sugary).map(|(u, _, _)| u),
        None => self.plain_usage.get_mut(&(w.expr, w.plain)),
      };
      match usage {
        None => {
          always!(false, "can't find usage count for {def:?} for {id:?}");
        }
        Some(usage) => usage.count += 1,
      }
    }
    Some(def)
  }

  fn undefine(&mut self, id: Id) {
    let prev_def = self.store.entry(id).or_default().pop();
    always!(prev_def.is_some(), "undefine without previous define: {id:?}");
  }
}

/// Performs the checks.
pub fn get(st: &mut St, ars: &Arenas, expr: Expr) {
  let mut cx = Cx::default();
  cx.define(Id::std, Def::Std);
  cx.define(Id::std_unutterable, Def::Std);
  check(st, &mut cx, ars, expr);
  cx.undefine(Id::std);
  cx.undefine(Id::std_unutterable);
  for (_, stack) in cx.store {
    always!(stack.is_empty());
  }
  for ((expr, plain), usage) in cx.plain_usage {
    if usage.count != 0 || usage.id == Id::dollar {
      continue;
    }
    let we = def::WithExpr { expr, plain, sugary: None };
    st.err(expr, error::Kind::Unused(usage.id, we));
  }
  for (sugary, (usage, expr, plain)) in cx.sugary_usage {
    if usage.count != 0 || usage.id == Id::dollar {
      continue;
    }
    let we = def::WithExpr { expr, plain, sugary: Some(sugary) };
    st.err(expr, error::Kind::Unused(usage.id, we));
  }
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
      cx.undefine(Id::self_);
      cx.undefine(Id::super_);
    }
    ExprData::ObjectComp { name, body, id, ary } => {
      check(st, cx, ars, *ary);
      let def = def::WithExpr { expr, plain: def::Plain::ObjectCompId, sugary: None };
      cx.define(*id, Def::Expr(def));
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
      for (idx, &(bind, rhs)) in binds.iter().enumerate() {
        let def =
          def::WithExpr { expr, plain: def::Plain::LocalBind(idx), sugary: bind.sugary_def };
        cx.define(bind.id, Def::Expr(def));
        if !bound_names.insert(bind.id) {
          st.err(rhs.unwrap_or(expr), error::Kind::DuplicateBinding(bind.id));
        }
      }
      for &(_, rhs) in binds {
        check(st, cx, ars, rhs);
      }
      check(st, cx, ars, *body);
      for &(bind, _) in binds {
        cx.undefine(bind.id);
      }
    }
    ExprData::Function { params, body } => {
      let mut bound_names = FxHashSet::<Id>::default();
      for (idx, &(bind, rhs)) in params.iter().enumerate() {
        let def = def::WithExpr { expr, plain: def::Plain::FnParam(idx), sugary: bind.sugary_def };
        cx.define(bind.id, Def::Expr(def));
        if !bound_names.insert(bind.id) {
          st.err(rhs.flatten().unwrap_or(expr), error::Kind::DuplicateBinding(bind.id));
        }
      }
      for &(_, rhs) in params {
        let Some(rhs) = rhs else { continue };
        check(st, cx, ars, rhs);
      }
      check(st, cx, ars, *body);
      for &(bind, _) in params {
        cx.undefine(bind.id);
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
