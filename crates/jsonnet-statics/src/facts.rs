//! Processing facts to determine the types of identifiers.
//!
//! Each fact about a variable `$var` must be one of:
//!
//! - `std.isTY($var)` where TY is one of Number, String, Boolean, Array, Object, or Function
//! - `std.type($var) == "S"` where S is one of number, string, boolean, array, object, function, or
//!   null
//! - `std.objectHas(All)($var, "FIELD")` where FIELD is a field name
//! - `$var == LIT` where LIT is some literal (`null`, `3`, `"hi"`, `false`, etc)
//!
//! caveats:
//!
//! - cannot do `local isNumber = std.isNumber` beforehand, must literally get the field off
//!   `std`
//! - support for named params to fns may be incomplete
//!
//! on the bright side:
//!
//! - can chain the facts with `a && b` (or `if a then b else false`, which is what `&&` desugars
//!   to)
//! - can chain the facts with `a || b` (or `if a then true else b`, which is what `||`
//!   desugars to); when both a and b are about the same variable, will union the types
//! - "FIELD" in $var desugars to a call to `std.objectHasAll`, so that works
//! - checking we get from `std` is NOT syntactic, we do an env lookup. so we won't trick this by
//!   doing `local std = wtf` beforehand, and also it'll still work with `local foo = std` and then
//!   asserting with `foo.isTY` etc.

use crate::scope::{Fact, Facts, Scope};
use jsonnet_expr::{Expr, ExprArena, ExprData, ExprMust, Id, Prim, Str};
use jsonnet_ty as ty;
use std::collections::BTreeMap;

/// Collects facts that are always true in the expression because otherwise the expression diverges
/// (i.e. it `error`s).
///
/// - asserts all be at the beginning of the expression, so cannot e.g. introduce new local
///   variables
/// - since asserts are lowered to `if cond then ... else error ...`, we check for that. so if the
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

/// Process facts from a single if-cond.
pub(crate) fn get_cond(
  tys: &mut ty::MutStore<'_>,
  scope: &Scope,
  ar: &ExprArena,
  ac: &mut Facts,
  cond: Expr,
) {
  let Some(cond) = cond else { return };
  match &ar[cond] {
    ExprData::Call { func: Some(func), positional, named } => {
      let ExprData::Subscript { on: Some(on), idx: Some(idx) } = ar[*func] else { return };
      let ExprData::Id(std_id) = &ar[on] else { return };
      if !scope.is_std(*std_id) {
        return;
      }
      let ExprData::Prim(Prim::String(func_name)) = &ar[idx] else { return };
      match *func_name {
        Str::isArray => get_is_ty(tys, ar, ac, positional, named, ty::Ty::ARRAY_ANY),
        Str::isBoolean => get_is_ty(tys, ar, ac, positional, named, ty::Ty::BOOL),
        Str::isNumber => get_is_ty(tys, ar, ac, positional, named, ty::Ty::NUMBER),
        Str::isObject => get_is_ty(tys, ar, ac, positional, named, ty::Ty::OBJECT),
        Str::isString => get_is_ty(tys, ar, ac, positional, named, ty::Ty::STRING),
        Str::isFunction => get_is_ty(tys, ar, ac, positional, named, ty::Ty::UNKNOWN_FN),
        Str::objectHas | Str::objectHasAll => {
          // TODO handle named params (including mixed positional/named)
          let (&[Some(obj), Some(field)], []) = (&positional[..], &named[..]) else { return };
          let ExprData::Prim(Prim::String(field)) = &ar[field] else { return };
          let mut ty = tys.get(ty::Data::Object(ty::Object {
            known: BTreeMap::from([(field.clone(), ty::Ty::ANY)]),
            has_unknown: true,
          }));
          let Some(id) = follow_subscripts(tys, ar, obj, &mut ty) else { return };
          ac.add(tys, id, Fact::total(ty));
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
          let Some(&snd) = snd.get(id) else { continue };
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
        ac.add(tys, id, fact.negate());
      }
    }
    _ => {}
  }
}

/// Handles a call to a `std.isTY` function.
fn get_is_ty(
  tys: &mut ty::MutStore<'_>,
  ar: &ExprArena,
  ac: &mut Facts,
  positional: &[Expr],
  named: &[(Id, Expr)],
  mut ty: ty::Ty,
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
  let Some(id) = follow_subscripts(tys, ar, param, &mut ty) else { return };
  ac.add(tys, id, Fact::total(ty));
}

/// Process `std.type($var) == "TYPE"`, where TYPE is number, string, etc.
fn get_ty_eq(
  tys: &mut ty::MutStore<'_>,
  scope: &Scope,
  ar: &ExprArena,
  ac: &mut Facts,
  call: ExprMust,
  type_str: ExprMust,
) {
  let ExprData::Call { func: Some(func), positional, named } = &ar[call] else { return };
  let ExprData::Subscript { on: Some(on), idx: Some(idx) } = ar[*func] else { return };
  let ExprData::Id(std_id) = &ar[on] else { return };
  if !scope.is_std(*std_id) {
    return;
  }
  let ExprData::Prim(Prim::String(func_name)) = &ar[idx] else { return };
  if *func_name != Str::type_ {
    return;
  }
  let param = match (&positional[..], &named[..]) {
    (&[Some(x)], []) => x,
    ([], &[(id, Some(x))]) => {
      if id != Id::x {
        return;
      }
      x
    }
    _ => return,
  };
  let ExprData::Prim(Prim::String(type_str)) = &ar[type_str] else { return };
  let mut ty = match *type_str {
    Str::array => ty::Ty::ARRAY_ANY,
    Str::boolean => ty::Ty::BOOL,
    Str::number => ty::Ty::NUMBER,
    Str::object => ty::Ty::OBJECT,
    Str::string => ty::Ty::STRING,
    Str::function => ty::Ty::UNKNOWN_FN,
    Str::null => ty::Ty::NULL,
    _ => return,
  };
  let Some(id) = follow_subscripts(tys, ar, param, &mut ty) else { return };
  ac.add(tys, id, Fact::total(ty));
}

/// Process `$var == LIT`, where LIT is some literal.
fn get_eq_lit(
  tys: &mut ty::MutStore<'_>,
  ar: &ExprArena,
  ac: &mut Facts,
  var: ExprMust,
  lit: ExprMust,
) {
  let (mut ty, partial) = match &ar[lit] {
    ExprData::Prim(prim) => match prim {
      Prim::Null => (ty::Ty::NULL, false),
      Prim::Bool(b) => (if *b { ty::Ty::TRUE } else { ty::Ty::FALSE }, false),
      Prim::String(_) => (ty::Ty::STRING, true),
      Prim::Number(_) => (ty::Ty::NUMBER, true),
    },
    ExprData::Object { .. } | ExprData::ObjectComp { .. } => (ty::Ty::OBJECT, true),
    ExprData::Array(_) => (ty::Ty::ARRAY_ANY, true),
    ExprData::Error(_) => (ty::Ty::NEVER, false),
    _ => return,
  };
  let Some(id) = follow_subscripts(tys, ar, var, &mut ty) else { return };
  ac.add(tys, id, Fact::with_partiality(ty, partial));
}

/// follow subscripts towards an id at the end, updating the ty as we go.
fn follow_subscripts(
  tys: &mut ty::MutStore<'_>,
  ar: &ExprArena,
  mut expr: ExprMust,
  ty: &mut ty::Ty,
) -> Option<Id> {
  loop {
    match ar[expr] {
      ExprData::Subscript { on: Some(on), idx: Some(idx) } => {
        let ExprData::Prim(Prim::String(idx)) = &ar[idx] else { return None };
        *ty = tys.get(ty::Data::Object(ty::Object {
          known: BTreeMap::from([(idx.clone(), *ty)]),
          has_unknown: true,
        }));
        expr = on;
      }
      ExprData::Id(id) => return Some(id),
      _ => return None,
    }
  }
}
