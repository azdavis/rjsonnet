//! A lazy evaluator for jsonnet.

#![deny(clippy::pedantic, missing_debug_implementations, missing_docs, rust_2018_idioms)]
#![allow(dead_code)]

use la_arena::{Arena, Idx};
use rustc_hash::FxHashSet;

type Expr = Idx<ExprD>;
type ExprArena = Arena<ExprD>;

/// An expression in the core language.
///
/// The D stands for "Data".
#[derive(Debug)]
enum ExprD {
  Null,
  True,
  False,
  Self_,
  Super,
  String(Str),
  Number(f64),
  Object { asserts: Vec<Expr>, fields: Vec<(Expr, Hidden, Expr)> },
  ObjectComp { key: Expr, val: Expr, id: Id, iter: Expr },
  Array(Vec<Expr>),
  Subscript(Expr, Expr),
  Call { func: Expr, positional: Vec<Expr>, named: Vec<(Id, Expr)> },
  Id(Id),
  Local(Vec<(Id, Expr)>, Expr),
  If(Expr, Expr, Expr),
  BinaryOp(Expr, BinaryOp, Expr),
  UnaryOp(UnaryOp, Expr),
  Function(Vec<(Id, Expr)>, Expr),
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
struct Id(u32);

impl Id {
  const STD: Self = Id(0);
  const SELF: Self = Id(1);
  const SUPER: Self = Id(2);
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
  fn contains(&self, id: Id) -> bool {
    self.store.contains(&id)
  }

  fn insert(&mut self, id: Id) {
    self.store.insert(id);
  }
}

struct Arenas {
  str: StrArena,
  expr: ExprArena,
}

fn check(st: &mut St, cx: &Cx, ars: &Arenas, expr: Expr) {
  match &ars.expr[expr] {
    ExprD::Null | ExprD::True | ExprD::False | ExprD::String(_) | ExprD::Number(_) => {}
    ExprD::Self_ => {
      if !cx.contains(Id::SELF) {
        st.err(expr, "`self` not in scope");
      }
    }
    ExprD::Super => {
      if !cx.contains(Id::SUPER) {
        st.err(expr, "`super` not in scope");
      }
    }
    ExprD::Object { asserts, fields } => {
      let cx_big = {
        let mut cx = cx.clone();
        cx.insert(Id::SELF);
        cx.insert(Id::SUPER);
        cx
      };
      let mut string_fields = FxHashSet::<Str>::default();
      for &(key, _, val) in fields {
        check(st, cx, ars, key);
        check(st, &cx_big, ars, val);
        if let ExprD::String(s) = ars.expr[key] {
          if !string_fields.insert(s) {
            st.err(key, "duplicate field");
          }
        }
      }
      for &cond in asserts {
        check(st, &cx_big, ars, cond);
      }
    }
    ExprD::ObjectComp { key, val, id, iter } => {
      check(st, cx, ars, *iter);
      let mut cx = cx.clone();
      cx.insert(*id);
      check(st, &cx, ars, *key);
      cx.insert(Id::SELF);
      cx.insert(Id::SUPER);
      check(st, &cx, ars, *val);
    }
    ExprD::Array(exprs) => {
      for &arg in exprs {
        check(st, cx, ars, arg);
      }
    }
    ExprD::Subscript(ary, idx) => {
      check(st, cx, ars, *ary);
      check(st, cx, ars, *idx);
    }
    ExprD::Call { func, positional, named } => {
      check(st, cx, ars, *func);
      for &arg in positional {
        check(st, cx, ars, arg);
      }
      let mut named_args = FxHashSet::<Id>::default();
      for &(id, arg) in named {
        check(st, cx, ars, arg);
        if !named_args.insert(id) {
          // TODO move err to the id, not the arg
          st.err(arg, "duplicate named argument");
        }
      }
    }
    ExprD::Id(id) => {
      if !cx.contains(*id) {
        st.err(expr, "identifier not in scope");
      }
    }
    // turns out these are exactly the same
    ExprD::Local(binds, body) | ExprD::Function(binds, body) => {
      let mut cx = cx.clone();
      let mut bound_ids = FxHashSet::<Id>::default();
      for &(id, rhs) in binds {
        cx.insert(id);
        if !bound_ids.insert(id) {
          // TODO move err to the id, not the rhs
          st.err(rhs, "duplicate binding");
        }
      }
      for &(_, rhs) in binds {
        check(st, &cx, ars, rhs);
      }
      check(st, &cx, ars, *body);
    }
    ExprD::If(cond, yes, no) => {
      check(st, cx, ars, *cond);
      check(st, cx, ars, *yes);
      check(st, cx, ars, *no);
    }
    ExprD::BinaryOp(lhs, _, rhs) => {
      check(st, cx, ars, *lhs);
      check(st, cx, ars, *rhs);
    }
    ExprD::UnaryOp(_, inner) => {
      check(st, cx, ars, *inner);
    }
    ExprD::Error(inner) => {
      check(st, cx, ars, *inner);
    }
  }
}
