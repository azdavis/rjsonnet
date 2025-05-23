//! Extracting the facts from expressions.
//!
//! Note: cannot do e.g. `local isNumber = std.isNumber` beforehand, must literally get the field
//! off `std`. On the bright side, checking we get from `std` is NOT syntactic, we do an env lookup.
//! so we won't trick this by doing `local std = wtf` beforehand, and also it'll still work with
//! `local foo = std` and then asserting with `foo.isTY` etc.

use crate::flow::data::{Fact, Facts, Totality};
use crate::scope::Scope;
use jsonnet_expr::{Expr, ExprArena, ExprData, ExprMust, Prim, Str};

/// Collects facts that are always true in the expression because otherwise the expression diverges
/// (i.e. it `error`s).
///
/// - asserts must all be at the beginning of the expression, so cannot e.g. introduce new local
///   variables
/// - since `assert c; e` is lowered to `if c then e else error ...`, we check for that. so if the
///   user wrote that itself in the concrete syntax, that also works.
pub(crate) fn get_always(scope: &Scope, ar: &ExprArena, body: Expr) -> Facts {
  let mut ac = Facts::default();
  let Some(body) = body else { return ac };
  if let ExprData::Object { asserts, .. } = &ar[body] {
    for &a in asserts {
      get_assert(scope, ar, &mut ac, a);
    }
  } else {
    get_assert(scope, ar, &mut ac, Some(body));
  }
  ac
}

/// Collects facts from desugared `assert`s, i.e. `if`s where the `else` diverges.
fn get_assert(scope: &Scope, ar: &ExprArena, ac: &mut Facts, mut body: Expr) {
  while let Some(mut b) = body {
    // this shenanigans with the local is especially useful for asserts inside objects, since we
    // desugar object locals into each assert and field. this pattern is likely applicable in more
    // situations than this one, but this is the most important.
    let mut bs = None::<&[(jsonnet_expr::Id, Expr)]>;
    if let ExprData::Local { binds, body: Some(body) } = &ar[b] {
      b = *body;
      bs = Some(binds.as_slice());
    }
    let ExprData::If { cond, yes, no: Some(no) } = ar[b] else { break };
    let ExprData::Error(_) = &ar[no] else { break };
    body = yes;
    get_cond(scope, ar, ac, cond);
    if let Some(bs) = bs {
      for &(id, _) in bs {
        ac.remove(id);
      }
    }
  }
}

/// Collects facts from a single `if` condition.
pub(crate) fn get_cond(scope: &Scope, ar: &ExprArena, ac: &mut Facts, cond: Expr) {
  let Some(cond) = cond else { return };
  match &ar[cond] {
    ExprData::Call { func: Some(func), positional: pos, named } => {
      let Some(func_name) = std_field(scope, ar, *func) else { return };
      if let Some(fact) = unary_std_fn_fact(func_name) {
        let (&[Some(param)], []) = (&pos[..], &named[..]) else { return };
        add_expr_fact(ar, ac, param, fact);
      } else {
        match func_name {
          Str::objectHas | Str::objectHasAll => {
            let (&[Some(obj), Some(field)], []) = (&pos[..], &named[..]) else { return };
            let ExprData::Prim(Prim::String(field)) = ar[field] else { return };
            let fact = Fact::has_field(field);
            add_expr_fact(ar, ac, obj, fact);
          }
          Str::objectHasEx => {
            let (&[Some(obj), Some(field), Some(_)], []) = (&pos[..], &named[..]) else { return };
            let ExprData::Prim(Prim::String(field)) = ar[field] else { return };
            let fact = Fact::has_field(field);
            add_expr_fact(ar, ac, obj, fact);
          }
          Str::all => {
            let (&[Some(arg)], []) = (&pos[..], &named[..]) else { return };
            get_all(scope, ar, ac, arg);
          }
          _ => {}
        }
      }
    }
    // the `if` condition is itself another `if` expression, which is actually common due to the
    // desugaring of `&&` and `||`
    &ExprData::If { cond, yes: Some(yes), no: Some(no) } => {
      if let ExprData::Prim(Prim::Bool(false)) | ExprData::Error(_) = &ar[no] {
        // it looks like a desugared `&&`, so do both in sequence.
        get_cond(scope, ar, ac, cond);
        get_cond(scope, ar, ac, Some(yes));
      } else if let ExprData::Prim(Prim::Bool(true)) = &ar[yes] {
        // it looks like a desugared `||`, so do the union.
        let mut fst = Facts::default();
        let mut snd = Facts::default();
        get_cond(scope, ar, &mut fst, cond);
        get_cond(scope, ar, &mut snd, Some(no));
        for (id, fst) in fst.into_iter() {
          // we require both sides of the `||` have specific facts about the same variable, since
          // the "base-case" fact is that it is anything, which isn't that useful.
          let Some(snd) = snd.remove(id) else { continue };
          let fact = fst.or(snd);
          ac.add(id, fact);
        }
      }
    }
    &ExprData::BinOp { lhs: Some(lhs), op: jsonnet_expr::BinOp::Eq, rhs: Some(rhs) } => {
      // for all of these, call each fn twice with lhs and rhs swapped. if one works, the other
      // won't, but we'll just return.
      //
      // `std.type`
      get_ty_eq(scope, ar, ac, lhs, rhs);
      get_ty_eq(scope, ar, ac, rhs, lhs);
      // `std.length`
      get_len_eq(scope, ar, ac, lhs, rhs);
      get_len_eq(scope, ar, ac, rhs, lhs);
      // literal
      get_eq_lit(ar, ac, lhs, rhs);
      get_eq_lit(ar, ac, rhs, lhs);
    }
    &ExprData::UnOp { op: jsonnet_expr::UnOp::LogicalNot, inner } => {
      let mut neg = Facts::default();
      get_cond(scope, ar, &mut neg, inner);
      for (id, fact) in neg.into_iter() {
        ac.add(id, fact.not());
      }
    }
    &ExprData::Id(id) => ac.add(id, Fact::true_()),
    _ => {}
  }
}

fn unary_std_fn_fact(fn_name: Str) -> Option<Fact> {
  let ret = match fn_name {
    Str::isArray => Fact::array(Totality::Total),
    Str::isBoolean => Fact::boolean(),
    Str::isNumber => Fact::number(Totality::Total),
    Str::isObject => Fact::object(Totality::Total),
    Str::isString => Fact::string(Totality::Total),
    Str::isFunction => Fact::function(),
    Str::isEven | Str::isOdd | Str::isInteger | Str::isDecimal => Fact::number(Totality::Partial),
    _ => return None,
  };
  Some(ret)
}

fn get_unary_std_fn_param(scope: &Scope, ar: &ExprArena, call: ExprMust, fn_name: Str) -> Expr {
  let ExprData::Call { func: Some(func), positional, named } = &ar[call] else { return None };
  if std_field(scope, ar, *func).is_none_or(|got| got != fn_name) {
    return None;
  }
  let (&[Some(param)], []) = (&positional[..], &named[..]) else { return None };
  Some(param)
}

fn std_field(scope: &Scope, ar: &ExprArena, func: ExprMust) -> Option<Str> {
  let ExprData::Subscript { on: Some(on), idx: Some(idx) } = ar[func] else { return None };
  let ExprData::Id(std_id) = &ar[on] else { return None };
  if !scope.is_std(*std_id) {
    return None;
  }
  let ExprData::Prim(Prim::String(name)) = ar[idx] else { return None };
  Some(name)
}

fn get_ty_eq(scope: &Scope, ar: &ExprArena, ac: &mut Facts, call: ExprMust, type_str: ExprMust) {
  let Some(param) = get_unary_std_fn_param(scope, ar, call, Str::type_) else { return };
  let ExprData::Prim(Prim::String(type_str)) = &ar[type_str] else { return };
  let fact = match *type_str {
    Str::array => Fact::array(Totality::Total),
    Str::boolean => Fact::boolean(),
    Str::number => Fact::number(Totality::Total),
    Str::object => Fact::object(Totality::Total),
    Str::string => Fact::string(Totality::Total),
    Str::function => Fact::function(),
    Str::null => Fact::null(),
    _ => return,
  };
  add_expr_fact(ar, ac, param, fact);
}

fn get_len_eq(scope: &Scope, ar: &ExprArena, ac: &mut Facts, call: ExprMust, n: ExprMust) {
  let Some(param) = get_unary_std_fn_param(scope, ar, call, Str::length) else { return };
  let ExprData::Prim(Prim::Number(n)) = &ar[n] else { return };
  let Some(n) = get_uint(n.value()) else { return };
  add_expr_fact(ar, ac, param, Fact::has_len(n));
}

/// Returns this as a usize if possible.
#[expect(clippy::float_cmp, clippy::cast_possible_truncation, clippy::cast_sign_loss)]
pub(crate) fn get_uint(n: f64) -> Option<usize> {
  if n >= 0.0 && n.trunc() == n { Some(n as usize) } else { None }
}

fn get_eq_lit(ar: &ExprArena, ac: &mut Facts, var: ExprMust, lit: ExprMust) {
  let fact = match &ar[lit] {
    ExprData::Prim(prim) => match prim {
      Prim::Null => Fact::null(),
      Prim::Bool(b) => {
        if *b {
          Fact::true_()
        } else {
          Fact::false_()
        }
      }
      Prim::String(_) => Fact::string(Totality::Partial),
      Prim::Number(_) => Fact::number(Totality::Partial),
    },
    ExprData::Array(_) => Fact::array(Totality::Partial),
    ExprData::Object { .. } | ExprData::ObjectComp { .. } => Fact::object(Totality::Partial),
    _ => return,
  };
  add_expr_fact(ar, ac, var, fact);
}

fn get_all(scope: &Scope, ar: &ExprArena, ac: &mut Facts, arg: ExprMust) {
  let ExprData::Call { func: Some(func), positional: pos, named } = &ar[arg] else { return };
  if std_field(scope, ar, *func).is_none_or(|got| got != Str::map) {
    return;
  }
  let (&[Some(map_fn), Some(array)], []) = (&pos[..], &named[..]) else { return };
  let Some(elem_fact) = get_predicate(scope, ar, map_fn) else { return };
  add_expr_fact(ar, ac, array, elem_fact.into_array());
}

pub(crate) fn get_predicate(scope: &Scope, ar: &ExprArena, func: ExprMust) -> Option<Fact> {
  if let Some(field) = std_field(scope, ar, func) {
    unary_std_fn_fact(field)
  } else if let ExprData::Fn { params, body } = &ar[func] {
    let &[(id, _)] = &params[..] else { return None };
    let mut body_ac = Facts::default();
    get_cond(scope, ar, &mut body_ac, *body);
    body_ac.remove(id)
  } else {
    None
  }
}

fn add_expr_fact(ar: &ExprArena, ac: &mut Facts, mut expr: ExprMust, fact: Fact) {
  let mut path = Vec::<Str>::new();
  let id = loop {
    match ar[expr] {
      ExprData::Subscript { on: Some(on), idx: Some(idx) } => {
        let ExprData::Prim(Prim::String(idx)) = ar[idx] else { return };
        path.push(idx);
        expr = on;
      }
      ExprData::Id(id) => break id,
      _ => return,
    }
  };
  let fact = fact.for_path(path);
  ac.add(id, fact);
}
