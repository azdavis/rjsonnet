//! Extracting the facts from expressions.
//!
//! Note: cannot do e.g. `local isNumber = std.isNumber` beforehand, must literally get the field
//! off `std`. On the bright side, checking we get from `std` is NOT syntactic, we do an env lookup.
//! so we won't trick this by doing `local std = wtf` beforehand, and also it'll still work with
//! `local foo = std` and then asserting with `foo.isTY` etc.

use crate::flow::data::{Fact, Facts, Totality};
use crate::scope::Scope;
use jsonnet_expr::{Expr, ExprArena, ExprData, ExprMust, Id, Prim, Str};

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
  if let ExprData::Object { binds, asserts, .. } = &ar[body] {
    for &a in asserts {
      get_assert(scope, ar, &mut ac, a);
    }
    for &(id, _) in binds {
      ac.remove(id);
    }
  } else {
    get_assert(scope, ar, &mut ac, Some(body));
  }
  ac
}

/// Collects facts from desugared `assert`s, i.e. `if`s where the `else` diverges.
fn get_assert(scope: &Scope, ar: &ExprArena, ac: &mut Facts, mut body: Expr) {
  while let Some(b) = body {
    let ExprData::If { cond, yes, no: Some(no) } = ar[b] else { break };
    let ExprData::Error(_) = &ar[no] else { break };
    body = yes;
    get_cond(scope, ar, ac, cond);
  }
}

/// Collects facts from a single `if` condition.
pub(crate) fn get_cond(scope: &Scope, ar: &ExprArena, ac: &mut Facts, cond: Expr) {
  let Some(cond) = cond else { return };
  match &ar[cond] {
    ExprData::Call { func: Some(func), positional: pos, named } => {
      let ExprData::Subscript { on: Some(on), idx: Some(idx) } = ar[*func] else { return };
      let ExprData::Id(std_id) = &ar[on] else { return };
      if !scope.is_std(*std_id) {
        return;
      }
      let ExprData::Prim(Prim::String(func_name)) = &ar[idx] else { return };
      match *func_name {
        Str::isArray => get_is_ty(ar, ac, pos, named, Fact::array(Totality::Total)),
        Str::isBoolean => get_is_ty(ar, ac, pos, named, Fact::boolean()),
        Str::isNumber => get_is_ty(ar, ac, pos, named, Fact::number(Totality::Total)),
        Str::isObject => get_is_ty(ar, ac, pos, named, Fact::object(Totality::Total)),
        Str::isString => get_is_ty(ar, ac, pos, named, Fact::string(Totality::Total)),
        Str::isFunction => get_is_ty(ar, ac, pos, named, Fact::function()),
        Str::isEven | Str::isOdd | Str::isInteger | Str::isDecimal => {
          get_is_ty(ar, ac, pos, named, Fact::number(Totality::Partial));
        }
        Str::objectHas | Str::objectHasAll => {
          // TODO handle named params (including mixed pos/named)
          let (&[Some(obj), Some(field)], []) = (&pos[..], &named[..]) else { return };
          let ExprData::Prim(Prim::String(field)) = &ar[field] else { return };
          let fact = Fact::has_field(field.clone());
          add_fact(ar, ac, obj, fact);
        }
        Str::objectHasEx => {
          // TODO handle named params (including mixed pos/named)
          let (&[Some(obj), Some(field), Some(_)], []) = (&pos[..], &named[..]) else { return };
          let ExprData::Prim(Prim::String(field)) = &ar[field] else { return };
          let fact = Fact::has_field(field.clone());
          add_fact(ar, ac, obj, fact);
        }
        _ => {}
      }
    }
    // the cond is itself another if expression.
    &ExprData::If { cond, yes: Some(yes), no: Some(no) } => {
      if let ExprData::Prim(Prim::Bool(false)) | ExprData::Error(_) = &ar[no] {
        // if it looks like a desugared `&&`, then just do both in sequence.
        get_cond(scope, ar, ac, cond);
        get_cond(scope, ar, ac, Some(yes));
      } else if let ExprData::Prim(Prim::Bool(true)) = &ar[yes] {
        // if it looks like a desugared `||`, then do the union.
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
    &ExprData::BinaryOp { lhs: Some(lhs), op: jsonnet_expr::BinaryOp::Eq, rhs: Some(rhs) } => {
      // do both sides. if one works, the other won't, but we'll just return. this allows for both
      // `std.type(x) == "TYPE"` and `"TYPE" == std.type(x)`.
      get_ty_eq(scope, ar, ac, lhs, rhs);
      get_ty_eq(scope, ar, ac, rhs, lhs);
      // same with this one.
      get_len_eq(scope, ar, ac, lhs, rhs);
      get_len_eq(scope, ar, ac, rhs, lhs);
      // and this one.
      get_eq_lit(ar, ac, lhs, rhs);
      get_eq_lit(ar, ac, rhs, lhs);
    }
    &ExprData::UnaryOp { op: jsonnet_expr::UnaryOp::LogicalNot, inner } => {
      let mut neg = Facts::default();
      get_cond(scope, ar, &mut neg, inner);
      for (id, fact) in neg.into_iter() {
        ac.add(id, fact.not());
      }
    }
    _ => {}
  }
}

/// Collects facts from call to a `std.isTYPE` function.
fn get_is_ty(
  ar: &ExprArena,
  ac: &mut Facts,
  positional: &[Expr],
  named: &[(Id, Expr)],
  fact: Fact,
) {
  let param = match (positional, named) {
    (&[Some(x)], []) => x,
    ([], &[(id, Some(x))]) => {
      if id != Id::v {
        return;
      }
      x
    }
    _ => return,
  };
  add_fact(ar, ac, param, fact);
}

fn get_std_fn_param(
  scope: &Scope,
  ar: &ExprArena,
  call: ExprMust,
  fn_name: &Str,
  param_name: Id,
) -> Expr {
  let ExprData::Call { func: Some(func), positional, named } = &ar[call] else { return None };
  let ExprData::Subscript { on: Some(on), idx: Some(idx) } = ar[*func] else { return None };
  let ExprData::Id(std_id) = &ar[on] else { return None };
  if !scope.is_std(*std_id) {
    return None;
  }
  let ExprData::Prim(Prim::String(func_name)) = &ar[idx] else { return None };
  if func_name != fn_name {
    return None;
  }
  let param = match (&positional[..], &named[..]) {
    (&[Some(x)], []) => x,
    ([], &[(id, Some(x))]) => {
      if id != param_name {
        return None;
      }
      x
    }
    _ => return None,
  };
  Some(param)
}

/// Collects facts from `std.type(expr) == STR`, where `STR` is `"number"`, `"string"`, etc.
fn get_ty_eq(scope: &Scope, ar: &ExprArena, ac: &mut Facts, call: ExprMust, type_str: ExprMust) {
  let Some(param) = get_std_fn_param(scope, ar, call, &Str::type_, Id::x) else { return };
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
  add_fact(ar, ac, param, fact);
}

fn get_len_eq(scope: &Scope, ar: &ExprArena, ac: &mut Facts, call: ExprMust, n: ExprMust) {
  let Some(param) = get_std_fn_param(scope, ar, call, &Str::length, Id::x) else { return };
  let ExprData::Prim(Prim::Number(n)) = &ar[n] else { return };
  let Some(n) = get_uint(n.value()) else { return };
  add_fact(ar, ac, param, Fact::has_len(n));
}

#[expect(clippy::float_cmp, clippy::cast_possible_truncation, clippy::cast_sign_loss)]
pub(crate) fn get_uint(n: f64) -> Option<usize> {
  if n >= 0.0 && n.trunc() == n {
    Some(n as usize)
  } else {
    None
  }
}

/// Collects facts from `expr == LIT`, where `LIT` is some literal (`null`, `3`, `"hi"`, etc).
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
  add_fact(ar, ac, var, fact);
}

fn add_fact(ar: &ExprArena, ac: &mut Facts, mut expr: ExprMust, fact: Fact) {
  let mut path = Vec::<Str>::new();
  let id = loop {
    match ar[expr] {
      ExprData::Subscript { on: Some(on), idx: Some(idx) } => {
        let ExprData::Prim(Prim::String(idx)) = &ar[idx] else { return };
        path.push(idx.clone());
        expr = on;
      }
      ExprData::Id(id) => break id,
      _ => return,
    }
  };
  let fact = fact.for_path(path);
  ac.add(id, fact);
}
