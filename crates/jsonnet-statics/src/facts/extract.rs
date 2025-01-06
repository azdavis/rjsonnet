//! Extracting the facts from expressions.
//!
//! Note: cannot do e.g. `local isNumber = std.isNumber` beforehand, must literally get the field
//! off `std`. On the bright side, checking we get from `std` is NOT syntactic, we do an env lookup.
//! so we won't trick this by doing `local std = wtf` beforehand, and also it'll still work with
//! `local foo = std` and then asserting with `foo.isTY` etc.

use crate::facts::data::{Fact, Facts};
use crate::scope::Scope;
use jsonnet_expr::{Expr, ExprArena, ExprData, ExprMust, Id, Prim, Str};
use jsonnet_ty as ty;
use std::collections::BTreeMap;

/// Collects facts that are always true in the expression because otherwise the expression diverges
/// (i.e. it `error`s).
///
/// - asserts must all be at the beginning of the expression, so cannot e.g. introduce new local
///   variables
/// - since `assert c; e` is lowered to `if c then e else error ...`, we check for that. so if the
///   user wrote that itself in the concrete syntax, that also works.
pub(crate) fn get_always(
  tys: &mut ty::MutStore<'_>,
  scope: &Scope,
  ar: &ExprArena,
  body: Expr,
) -> Facts {
  let mut ac = Facts::default();
  let Some(body) = body else { return ac };
  if let ExprData::Object { binds, asserts, .. } = &ar[body] {
    for &a in asserts {
      get_assert(tys, scope, ar, &mut ac, a);
    }
    for &(id, _) in binds {
      ac.remove(id);
    }
  } else {
    get_assert(tys, scope, ar, &mut ac, Some(body));
  }
  ac
}

/// Collects facts from desugared `assert`s, i.e. `if`s where the `else` diverges.
fn get_assert(
  tys: &mut ty::MutStore<'_>,
  scope: &Scope,
  ar: &ExprArena,
  ac: &mut Facts,
  mut body: Expr,
) {
  while let Some(b) = body {
    let ExprData::If { cond, yes, no: Some(no) } = ar[b] else { break };
    let ExprData::Error(_) = &ar[no] else { break };
    body = yes;
    get_cond(tys, scope, ar, ac, cond);
  }
}

/// Collects facts from a single `if` condition.
pub(crate) fn get_cond(
  tys: &mut ty::MutStore<'_>,
  scope: &Scope,
  ar: &ExprArena,
  ac: &mut Facts,
  cond: Expr,
) {
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
        Str::isArray => get_is_ty(tys, ar, ac, pos, named, ty::Ty::ARRAY_ANY),
        Str::isBoolean => get_is_ty(tys, ar, ac, pos, named, ty::Ty::BOOL),
        Str::isNumber => get_is_ty(tys, ar, ac, pos, named, ty::Ty::NUMBER),
        Str::isObject => get_is_ty(tys, ar, ac, pos, named, ty::Ty::OBJECT),
        Str::isString => get_is_ty(tys, ar, ac, pos, named, ty::Ty::STRING),
        Str::isFunction => get_is_ty(tys, ar, ac, pos, named, ty::Ty::UNKNOWN_FN),
        Str::objectHas | Str::objectHasAll => {
          // TODO handle named params (including mixed pos/named)
          let (&[Some(obj), Some(field)], []) = (&pos[..], &named[..]) else { return };
          let ExprData::Prim(Prim::String(field)) = &ar[field] else { return };
          let ty = tys.get(ty::Data::Object(ty::Object {
            known: BTreeMap::from([(field.clone(), ty::Ty::ANY)]),
            has_unknown: true,
          }));
          add_fact(tys, ar, ac, obj, Fact::ty(ty));
        }
        _ => {}
      }
    }
    // the cond is itself another if expression.
    &ExprData::If { cond, yes: Some(yes), no: Some(no) } => {
      if let ExprData::Prim(Prim::Bool(false)) | ExprData::Error(_) = &ar[no] {
        // if it looks like a desugared `&&`, then just do both in sequence.
        get_cond(tys, scope, ar, ac, cond);
        get_cond(tys, scope, ar, ac, Some(yes));
      } else if let ExprData::Prim(Prim::Bool(true)) = &ar[yes] {
        // if it looks like a desugared `||`, then do the union.
        let mut fst = Facts::default();
        let mut snd = Facts::default();
        get_cond(tys, scope, ar, &mut fst, cond);
        get_cond(tys, scope, ar, &mut snd, Some(no));
        for (id, fst) in fst.into_iter() {
          // we require both sides of the `||` have specific facts about the same variable, since
          // the "base-case" fact is that it is anything, which isn't that useful.
          let Some(snd) = snd.remove(id) else { continue };
          let fact = fst.or(tys, snd);
          ac.add(tys, id, fact);
        }
      }
    }
    &ExprData::BinaryOp { lhs: Some(lhs), op: jsonnet_expr::BinaryOp::Eq, rhs: Some(rhs) } => {
      // do both sides. if one works, the other won't, but we'll just return. this allows for both
      // `std.type(x) == "TYPE"` and `"TYPE" == std.type(x)`.
      get_ty_eq(tys, scope, ar, ac, lhs, rhs);
      get_ty_eq(tys, scope, ar, ac, rhs, lhs);
      // same with this one.
      get_eq_lit(tys, ar, ac, lhs, rhs);
      get_eq_lit(tys, ar, ac, rhs, lhs);
    }
    &ExprData::UnaryOp { op: jsonnet_expr::UnaryOp::LogicalNot, inner } => {
      let mut neg = Facts::default();
      get_cond(tys, scope, ar, &mut neg, inner);
      for (id, fact) in neg.into_iter() {
        ac.add(tys, id, fact.not());
      }
    }
    _ => {}
  }
}

/// Collects facts from call to a `std.isTYPE` function.
fn get_is_ty(
  tys: &mut ty::MutStore<'_>,
  ar: &ExprArena,
  ac: &mut Facts,
  positional: &[Expr],
  named: &[(Id, Expr)],
  ty: ty::Ty,
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
  add_fact(tys, ar, ac, param, Fact::ty(ty));
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
fn get_ty_eq(
  tys: &mut ty::MutStore<'_>,
  scope: &Scope,
  ar: &ExprArena,
  ac: &mut Facts,
  call: ExprMust,
  type_str: ExprMust,
) {
  let Some(param) = get_std_fn_param(scope, ar, call, &Str::type_, Id::x) else { return };
  let ExprData::Prim(Prim::String(type_str)) = &ar[type_str] else { return };
  let ty = match *type_str {
    Str::array => ty::Ty::ARRAY_ANY,
    Str::boolean => ty::Ty::BOOL,
    Str::number => ty::Ty::NUMBER,
    Str::object => ty::Ty::OBJECT,
    Str::string => ty::Ty::STRING,
    Str::function => ty::Ty::UNKNOWN_FN,
    Str::null => ty::Ty::NULL,
    _ => return,
  };
  add_fact(tys, ar, ac, param, Fact::ty(ty));
}

/// Collects facts from `expr == LIT`, where `LIT` is some literal (`null`, `3`, `"hi"`, etc).
fn get_eq_lit(
  tys: &mut ty::MutStore<'_>,
  ar: &ExprArena,
  ac: &mut Facts,
  var: ExprMust,
  lit: ExprMust,
) {
  let ExprData::Prim(prim) = &ar[lit] else { return };
  let ty = match prim {
    Prim::Null => ty::Ty::NULL,
    Prim::Bool(b) => {
      if *b {
        ty::Ty::TRUE
      } else {
        ty::Ty::FALSE
      }
    }
    Prim::String(_) | Prim::Number(_) => return,
  };
  add_fact(tys, ar, ac, var, Fact::ty(ty));
}

fn add_fact(
  tys: &mut ty::MutStore<'_>,
  ar: &ExprArena,
  ac: &mut Facts,
  mut expr: ExprMust,
  fact: Fact,
) {
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
  let fact = fact.for_path(tys, path);
  ac.add(tys, id, fact);
}
