//! A lazy evaluator for jsonnet.

#![deny(clippy::pedantic, missing_debug_implementations, missing_docs, rust_2018_idioms)]
#![allow(dead_code)]

use la_arena::{Arena, Idx};
use rustc_hash::{FxHashMap, FxHashSet};

#[derive(Debug, Clone, Copy)]
enum Prim {
  Null,
  True,
  False,
  String(Str),
  Number(f64),
}

type Expr = Idx<ExprData>;
type ExprArena = Arena<ExprData>;

#[derive(Debug)]
enum ExprData {
  Prim(Prim),
  Object { asserts: Vec<Expr>, fields: Vec<(Expr, Hidden, Expr)> },
  ObjectComp { key: Expr, val: Expr, id: Id, iter: Expr },
  Array(Vec<Expr>),
  Subscript { on: Expr, idx: Expr },
  Call { func: Expr, positional: Vec<Expr>, named: Vec<(Id, Expr)> },
  Id(Id),
  Local { binds: Vec<(Id, Expr)>, body: Expr },
  If(Expr, Expr, Expr),
  BinaryOp(Expr, BinaryOp, Expr),
  UnaryOp(UnaryOp, Expr),
  Function { params: Vec<(Id, Expr)>, body: Expr },
  Error(Expr),
}

#[derive(Debug, Clone, Copy)]
enum Hidden {
  One,
  Two,
  Three,
}

#[derive(Debug, Clone, Copy)]
enum BinaryOp {
  Star,
  Slash,
  Percent,
  Plus,
  Minus,
  LtLt,
  GtGt,
  Lt,
  LtEq,
  Gt,
  GtEq,
  EqEq,
  BangEq,
  In,
  And,
  Carat,
  Bar,
  AndAnd,
  BarBar,
}

#[derive(Debug, Clone, Copy)]
enum UnaryOp {
  Minus,
  Plus,
  Bang,
  Tilde,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Id {}

impl Id {
  const STD: Self = Self {};
  const SELF: Self = Self {};
  const SUPER: Self = Self {};
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Str {}

#[derive(Debug)]
struct StrArena {}

impl StrArena {
  fn insert(&mut self, _: String) -> Str {
    todo!()
  }

  fn get(&self, _: Str) -> &str {
    todo!()
  }
}

struct St {}

impl St {
  fn err(&mut self, _: Expr, _: &str) {}
}

#[derive(Debug, Clone)]
struct Cx {
  store: FxHashSet<Id>,
}

impl Cx {
  fn insert(&mut self, id: Id) {
    self.store.insert(id);
  }

  fn contains(&self, id: Id) -> bool {
    self.store.contains(&id)
  }
}

struct Arenas {
  str: StrArena,
  expr: ExprArena,
}

fn check(st: &mut St, cx: &Cx, ars: &Arenas, expr: Expr) {
  match &ars.expr[expr] {
    ExprData::Prim(_) => {}
    ExprData::Object { asserts, fields } => {
      let cx_big = {
        let mut cx = cx.clone();
        cx.insert(Id::SELF);
        cx.insert(Id::SUPER);
        cx
      };
      let mut field_names = FxHashSet::<Str>::default();
      for &(key, _, val) in fields {
        check(st, cx, ars, key);
        check(st, &cx_big, ars, val);
        if let ExprData::Prim(Prim::String(s)) = ars.expr[key] {
          if !field_names.insert(s) {
            st.err(key, "duplicate field");
          }
        }
      }
      for &cond in asserts {
        check(st, &cx_big, ars, cond);
      }
    }
    ExprData::ObjectComp { key, val, id, iter } => {
      check(st, cx, ars, *iter);
      let mut cx = cx.clone();
      cx.insert(*id);
      check(st, &cx, ars, *key);
      cx.insert(Id::SELF);
      cx.insert(Id::SUPER);
      check(st, &cx, ars, *val);
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
          // TODO move err to the id, not the arg
          st.err(arg, "duplicate named argument");
        }
      }
    }
    ExprData::Id(id) => {
      if !cx.contains(*id) {
        st.err(expr, "identifier not in scope");
      }
    }
    // turns out these are exactly the same
    ExprData::Local { binds, body } | ExprData::Function { params: binds, body } => {
      let mut cx = cx.clone();
      let mut bound_names = FxHashSet::<Id>::default();
      for &(id, rhs) in binds {
        cx.insert(id);
        if !bound_names.insert(id) {
          // TODO move err to the id, not the rhs
          st.err(rhs, "duplicate binding");
        }
      }
      for &(_, rhs) in binds {
        check(st, &cx, ars, rhs);
      }
      check(st, &cx, ars, *body);
    }
    ExprData::If(cond, yes, no) => {
      check(st, cx, ars, *cond);
      check(st, cx, ars, *yes);
      check(st, cx, ars, *no);
    }
    ExprData::BinaryOp(lhs, _, rhs) => {
      check(st, cx, ars, *lhs);
      check(st, cx, ars, *rhs);
    }
    ExprData::UnaryOp(_, inner) => {
      check(st, cx, ars, *inner);
    }
    ExprData::Error(inner) => {
      check(st, cx, ars, *inner);
    }
  }
}

#[derive(Debug, Clone)]
struct Env {}

impl Env {
  fn insert(&mut self, _: Id, _: Val) {
    todo!()
  }

  fn get(&self, _: Id) -> &Val {
    todo!()
  }
}

/// The spec uses eager substitution but I suspect this is prohibitively non-performant. So we
/// separate values into primitives and recursive values. Recursive values contain expressions,
/// because Jsonnet itself has lazy semantics.
///
/// Because of this, and also because we choose to implement substitution lazily (as opposed to the
/// spec which expresses the semantics with eager substitution), we must therefore also carry with
/// recursive values an environment in which to do lazy substitutions.
///
/// Note that implementing substitution lazily is not meant to break with the spec. The execution
/// should be semantically equivalent.
///
/// We also consider errors values.
#[derive(Debug, Clone)]
enum Val {
  Prim(Prim),
  Rec { env: Env, kind: RecValKind },
  Error(Str),
}

#[derive(Debug, Clone)]
enum RecValKind {
  Object { asserts: Vec<Expr>, fields: FxHashMap<Str, (Hidden, Expr)> },
  Function { params: FxHashMap<Id, Expr>, body: Expr },
  Array(Vec<Expr>),
}
