//! Processing facts to determine the types of identifiers.
//!
//! Each fact about a variable `$var` must be one of:
//!
//! - `std.FUNC($var)` where FUNC is one of `isNumber`, `isString`, `isBoolean`, `isArray`, or
//!   `isObject`
//! - `std.type($var) == "S"` where S is one of number, string, boolean, array, object, or null
//! - `$var == null`
//!
//! notably:
//!
//! - cannot use `std.isFunction`, the type system cannot model a function with totally unknown
//!   params. this wouldn't be that helpful anyway i suppose - if you don't know how many params,
//!   how can you call it?
//! - cannot do `local isNumber = std.isNumber` beforehand, must literally get the field off `std`
//! - cannot use named arguments, only positional arguments
//!
//! on the bright side:
//!
//! - can chain the facts with `a && b` (or `if a then b else false`, which is what `&&` desugars
//!   to)
//! - can chain the facts with `a || b` (or `if a then true else b`, which is what `||`
//!   desugars to); when both a and b are about the same variable, will union the types
//! - checking we get from `std` is NOT syntactic, we do an env lookup. so we won't trick this by
//!   doing `local std = wtf` beforehand, and also it'll still work with `local foo = std` and then
//!   asserting with `foo.isTYPE` etc.

use crate::scope::{Facts, Scope};
use jsonnet_expr::{Expr, ExprArena, ExprData, ExprMust, Id, Prim, Str};
use jsonnet_ty as ty;

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
  mut body: Expr,
) -> Facts {
  let mut ac = Facts::default();
  while let Some(b) = body {
    let ExprData::If { cond, yes, no: Some(no) } = ar[b] else { break };
    let ExprData::Error(_) = &ar[no] else { break };
    body = yes;
    get_cond(tys, scope, ar, &mut ac, cond);
  }
  ac
}

/// Process a fact from a single if-cond.
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
      let ty = match *func_name {
        Str::isArray => ty::Ty::ARRAY_ANY,
        Str::isBoolean => ty::Ty::BOOL,
        Str::isNumber => ty::Ty::NUMBER,
        Str::isObject => ty::Ty::OBJECT,
        Str::isString => ty::Ty::STRING,
        _ => return,
      };
      if !named.is_empty() {
        return;
      }
      let [Some(param)] = positional[..] else { return };
      let ExprData::Id(id) = ar[param] else { return };
      add_fact(tys, ac, id, ty);
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
        for (id, fst_ty) in fst {
          let Some(&snd_ty) = snd.get(&id) else { continue };
          let ty = tys.get(ty::Data::Union(ty::Union::from([fst_ty, snd_ty])));
          add_fact(tys, ac, id, ty);
        }
      }
    }
    &ExprData::BinaryOp { lhs: Some(lhs), op: jsonnet_expr::BinaryOp::Eq, rhs: Some(rhs) } => {
      // do both sides. if one works, the other won't, but we'll just return. this allows for both
      // `std.type(x) == "TYPE"` and `"TYPE" == std.type(x)`.
      get_ty_eq(tys, scope, ar, ac, lhs, rhs);
      get_ty_eq(tys, scope, ar, ac, rhs, lhs);
      // same with this one.
      get_eq_lit(ar, ac, lhs, rhs);
      get_eq_lit(ar, ac, rhs, lhs);
    }
    _ => {}
  }
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
  if !named.is_empty() {
    return;
  }
  let [Some(param)] = positional[..] else { return };
  let ExprData::Id(id) = ar[param] else { return };
  let ExprData::Prim(Prim::String(type_str)) = &ar[type_str] else { return };
  let ty = match *type_str {
    Str::array => ty::Ty::ARRAY_ANY,
    Str::boolean => ty::Ty::BOOL,
    Str::number => ty::Ty::NUMBER,
    Str::object => ty::Ty::OBJECT,
    Str::string => ty::Ty::STRING,
    // as a little bonus.
    Str::null => ty::Ty::NULL,
    _ => return,
  };
  add_fact(tys, ac, id, ty);
}

/// Process `$var == LIT`, where LIT is some literal.
fn get_eq_lit(ar: &ExprArena, ac: &mut Facts, var: ExprMust, lit: ExprMust) {
  let ExprData::Id(param) = ar[var] else { return };
  let ExprData::Prim(Prim::Null) = &ar[lit] else { return };
  ac.entry(param).or_insert(ty::Ty::NULL);
}

fn add_fact(tys: &mut ty::MutStore<'_>, ac: &mut Facts, id: Id, ty: ty::Ty) {
  let entry = ac.entry(id).or_insert(ty::Ty::ANY);
  *entry = ty::logic::and(tys, *entry, ty);
}
