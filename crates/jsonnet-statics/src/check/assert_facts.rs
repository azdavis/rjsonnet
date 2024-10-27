//! See [`get`].

use crate::st;
use jsonnet_expr::{Expr, ExprArena, ExprData, ExprMust, Id, Prim, Str};
use jsonnet_ty as ty;

pub(crate) type Facts = rustc_hash::FxHashMap<Id, ty::Ty>;

/// collects facts from `asserts`s in an expression, to determine the types of identifiers.
///
/// note that this requires a very, very exact format for the asserts. each assert must be of the
/// form:
///
/// ```jsonnet
/// assert std.isTYPE(x);
/// ```
///
/// where TYPE is one of
///
/// - Number
/// - String
/// - Boolean
/// - Array
/// - Object
///
/// notably:
///
/// - cannot use `std.isFunction`, the type system cannot model a function with totally unknown
///   params. this wouldn't be that helpful anyway i suppose - if you don't know how many params,
///   how can you call it?
/// - cannot chain the asserts with `||` - no complex logic with ors
/// - asserts all be at the beginning of the fn, so cannot e.g. introduce new local variables
/// - cannot do `local isNumber = std.isNumber` beforehand, must literally get the field off `std`
/// - cannot use named arguments, only positional arguments
/// - isn't that clever with 'and'-ing facts about the same variable together: e.g. if we assert x
///   is a string, then x is a boolean, we'll just say x is a string, instead of noticing this will
///   always raise.
///
/// on the bright side:
///
/// - can chain the asserts with `a && b` (or `if a then b else false`, which is what `&&` desugars
///   to)
/// - can also use `std.type(x) == "TYPE"` where TYPE is number, string, etc.
/// - since asserts are lowered to `if cond then ... else error ...`, we check for that. so if the
///   user wrote that itself in the concrete syntax, that also works.
/// - checking we get from `std` is NOT syntactic, we do an env lookup. so we won't trick this by
///   doing `local std = wtf` beforehand, and also it'll still work with `local foo = std` and then
///   asserting with `foo.isTYPE` etc.
pub(crate) fn get(st: &st::St<'_>, ar: &ExprArena, mut body: Expr) -> Facts {
  let mut ac = Facts::default();
  while let Some(b) = body {
    let ExprData::If { cond, yes, no: Some(no) } = ar[b] else { break };
    let ExprData::Error(_) = &ar[no] else { break };
    body = yes;
    get_cond(st, ar, &mut ac, cond);
  }
  ac
}

/// process a single if-cond.
fn get_cond(st: &st::St<'_>, ar: &ExprArena, ac: &mut Facts, cond: Expr) {
  let Some(cond) = cond else { return };
  match &ar[cond] {
    ExprData::Call { func: Some(func), positional, named } => {
      let ExprData::Subscript { on: Some(on), idx: Some(idx) } = ar[*func] else { return };
      let ExprData::Id(std_id) = &ar[on] else { return };
      if !st.is_std(*std_id) {
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
      let ExprData::Id(param) = ar[param] else { return };
      ac.entry(param).or_insert(ty);
    }
    // the cond is itself another cond.
    &ExprData::If { cond, yes: Some(yes), no: Some(no) } => {
      if let ExprData::Prim(Prim::Bool(false)) | ExprData::Error(_) = &ar[no] {
        // if it looks like a desugared `&&`, then just do both in sequence.
        get_cond(st, ar, ac, cond);
        get_cond(st, ar, ac, Some(yes));
      }
    }
    &ExprData::BinaryOp { lhs: Some(lhs), op: jsonnet_expr::BinaryOp::Eq, rhs: Some(rhs) } => {
      // do both sides. if one works, the other won't, but we'll just return. this allows for both
      // `std.type(x) == "TYPE"` and `"TYPE" == std.type(x)`.
      get_ty_eq(st, ar, ac, lhs, rhs);
      get_ty_eq(st, ar, ac, rhs, lhs);
      // same with this one.
      get_eq_lit(ar, ac, lhs, rhs);
      get_eq_lit(ar, ac, rhs, lhs);
    }
    _ => {}
  }
}

/// process `std.type(x) == "TYPE"`, where TYPE is number, string, etc.
fn get_ty_eq(st: &st::St<'_>, ar: &ExprArena, ac: &mut Facts, call: ExprMust, type_str: ExprMust) {
  let ExprData::Call { func: Some(func), positional, named } = &ar[call] else { return };
  let ExprData::Subscript { on: Some(on), idx: Some(idx) } = ar[*func] else { return };
  let ExprData::Id(std_id) = &ar[on] else { return };
  if !st.is_std(*std_id) {
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
  let ExprData::Id(param) = ar[param] else { return };
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
  ac.entry(param).or_insert(ty);
}

/// process `x == Y`, where Y is some literal.
fn get_eq_lit(ar: &ExprArena, ac: &mut Facts, var: ExprMust, lit: ExprMust) {
  let ExprData::Id(param) = ar[var] else { return };
  let ExprData::Prim(Prim::Null) = &ar[lit] else { return };
  ac.entry(param).or_insert(ty::Ty::NULL);
}
