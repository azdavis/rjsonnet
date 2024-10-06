//! Check expressions for static validity: variables are in scope, types match up, etc.

mod call;

use crate::{error, st};
use always::always;
use jsonnet_expr::{def, BinaryOp, Expr, ExprArena, ExprData, Id, Prim, UnaryOp};
use jsonnet_ty as ty;
use rustc_hash::{FxHashMap, FxHashSet};
use std::collections::BTreeSet;

/// NOTE: don't return early from this except in the degenerate case where the `expr` was `None`.
/// This is so we can insert the expr's type into the `St` at the end.
#[expect(clippy::too_many_lines)]
pub(crate) fn get(st: &mut st::St<'_>, ar: &ExprArena, expr: Expr) -> ty::Ty {
  let Some(expr) = expr else { return ty::Ty::ANY };
  let ret = match &ar[expr] {
    ExprData::Prim(prim) => match prim {
      Prim::Null => ty::Ty::NULL,
      Prim::Bool(b) => {
        if *b {
          ty::Ty::TRUE
        } else {
          ty::Ty::FALSE
        }
      }
      Prim::String(_) => ty::Ty::STRING,
      Prim::Number(_) => ty::Ty::NUMBER,
    },
    ExprData::Object { binds, asserts, fields } => {
      let mut obj = ty::Object::empty();
      for field in fields {
        get(st, ar, field.key);
        let Some(key) = field.key else { continue };
        let ExprData::Prim(Prim::String(s)) = &ar[key] else {
          obj.has_unknown = true;
          continue;
        };
        if obj.known.insert(s.clone(), ty::Ty::ANY).is_some() {
          st.err(key, error::Kind::DuplicateFieldName(s.clone()));
        }
      }
      st.define_self_super();
      define_binds(st, ar, expr, binds, def::ExprDefKindMulti::ObjectLocalBind);
      for field in fields {
        let ty = get(st, ar, field.val);
        let Some(key) = field.key else { continue };
        let ExprData::Prim(Prim::String(s)) = &ar[key] else { continue };
        always!(obj.known.insert(s.clone(), ty).is_some());
      }
      for &cond in asserts {
        get(st, ar, cond);
      }
      st.undefine_self_super();
      for &(lhs, _) in binds {
        st.undefine(lhs);
      }
      st.get_ty(ty::Data::Object(obj))
    }
    ExprData::ObjectComp { name, body, id, ary } => {
      get(st, ar, *ary);
      st.define(*id, ty::Ty::ANY, def::Def::Expr(expr, def::ExprDefKind::ObjectCompId));
      get(st, ar, *name);
      st.define_self_super();
      get(st, ar, *body);
      st.undefine(*id);
      st.undefine_self_super();
      ty::Ty::OBJECT
    }
    ExprData::Array(exprs) => {
      let mut tys = BTreeSet::<ty::Ty>::new();
      for &arg in exprs {
        let ty = get(st, ar, arg);
        tys.insert(ty);
      }
      let elem_ty = st.get_ty(ty::Data::Union(tys));
      st.get_ty(ty::Data::Array(elem_ty))
    }
    ExprData::Subscript { on, idx } => {
      let on_ty = get(st, ar, *on);
      let idx_ty = get(st, ar, *idx);
      let idx_expr = idx.unwrap_or(expr);
      // TODO handle unions
      match st.data(on_ty).clone() {
        ty::Data::Array(elem_ty) => {
          st.unify(idx_expr, ty::Ty::NUMBER, idx_ty);
          elem_ty
        }
        ty::Data::Object(obj) => {
          st.unify(idx_expr, ty::Ty::STRING, idx_ty);
          let idx = idx.and_then(|x| match &ar[x] {
            ExprData::Prim(Prim::String(s)) => Some(s),
            _ => None,
          });
          match idx {
            // we do know what field we're asking for.
            Some(s) => {
              if let Some(&ty) = obj.known.get(s) {
                // we know the type of that field.
                ty
              } else {
                // we don't know the type.
                if !obj.has_unknown {
                  // this would result in a eval-time error if evaluated. warn statically.
                  st.err(idx_expr, error::Kind::MissingField(s.clone()));
                }
                ty::Ty::ANY
              }
            }
            // we don't know what field we're asking for.
            None => {
              if obj.has_unknown {
                // all bets are off.
                ty::Ty::ANY
              } else {
                // we know it has to be one of the known fields, but we don't know which one.
                st.get_ty(ty::Data::Union(obj.known.values().copied().collect()))
              }
            }
          }
        }
        _ => {
          st.unify(on.unwrap_or(expr), ty::Ty::ARRAY_OR_OBJECT, on_ty);
          ty::Ty::ANY
        }
      }
    }
    ExprData::Call { func, positional, named } => {
      let fn_ty = get(st, ar, *func);
      let pos_args: Vec<_> = positional.iter().map(|&arg| (arg, get(st, ar, arg))).collect();
      let mut named_args = FxHashMap::<Id, (Expr, ty::Ty)>::default();
      for &(id, arg) in named {
        let arg_ty = get(st, ar, arg);
        if named_args.insert(id, (arg, arg_ty)).is_some() {
          if let Some(arg) = arg {
            st.err(arg, error::Kind::DuplicateNamedArg(id));
          }
        }
      }
      call::get(st, expr, *func, fn_ty, &pos_args, &named_args)
    }
    ExprData::Id(id) => {
      if let Some((ty, def)) = st.get(*id) {
        st.note_usage(expr, def);
        ty
      } else {
        st.err(expr, error::Kind::NotInScope(*id));
        ty::Ty::ANY
      }
    }
    ExprData::Local { binds, body } => {
      define_binds(st, ar, expr, binds, def::ExprDefKindMulti::LocalBind);
      let ty = get(st, ar, *body);
      for &(bind, _) in binds {
        st.undefine(bind);
      }
      ty
    }
    ExprData::Function { params, body } => {
      let m = def::ExprDefKindMulti::FnParam;
      let param_tys = {
        let mut tmp = FxHashMap::<Id, ty::Ty>::default();
        for (idx, &(id, rhs)) in params.iter().enumerate() {
          if tmp.insert(id, ty::Ty::ANY).is_some() {
            st.err(rhs.flatten().unwrap_or(expr), error::Kind::DuplicateBinding(id, idx, m));
          }
        }
        refine_param_tys(st, ar, &mut tmp, *body);
        tmp
      };
      for (idx, &(id, _)) in params.iter().enumerate() {
        // unwrap_or should never happen, but if it does we'll notice with the always!(false) later
        let ty = param_tys.get(&id).copied().unwrap_or(ty::Ty::ANY);
        st.define(id, ty, def::Def::Expr(expr, def::ExprDefKind::Multi(idx, m)));
      }
      for &(_, rhs) in params {
        let Some(rhs) = rhs else { continue };
        get(st, ar, rhs);
      }
      let body_ty = get(st, ar, *body);
      let mut fn_params = Vec::<ty::Param>::with_capacity(params.len());
      for &(id, rhs) in params {
        st.undefine(id);
        let Some(&ty) = param_tys.get(&id) else {
          always!(false, "should have gotten fn param ty: {id:?}");
          continue;
        };
        fn_params.push(ty::Param { id, ty, required: rhs.is_none() });
      }
      let fn_ty = ty::RegularFn { params: fn_params, ret: body_ty };
      st.get_ty(ty::Data::Fn(ty::Fn::Regular(fn_ty)))
    }
    ExprData::If { cond, yes, no } => {
      let cond_ty = get(st, ar, *cond);
      st.unify(cond.unwrap_or(expr), ty::Ty::BOOL, cond_ty);
      let yes_ty = get(st, ar, *yes);
      let no_ty = get(st, ar, *no);
      st.get_ty(ty::Data::Union(BTreeSet::from([yes_ty, no_ty])))
    }
    ExprData::BinaryOp { lhs, op, rhs } => {
      let lhs_ty = get(st, ar, *lhs);
      let rhs_ty = get(st, ar, *rhs);
      match op {
        BinaryOp::Add => {
          match (st.data(lhs_ty), st.data(rhs_ty)) {
            (ty::Data::Prim(ty::Prim::Any), _) | (_, ty::Data::Prim(ty::Prim::Any)) => ty::Ty::ANY,
            // add numbers.
            (ty::Data::Prim(ty::Prim::Number), ty::Data::Prim(ty::Prim::Number)) => ty::Ty::NUMBER,
            // if any operand is string, coerce the other to string.
            (ty::Data::Prim(ty::Prim::String), _) | (_, ty::Data::Prim(ty::Prim::String)) => {
              ty::Ty::STRING
            }
            // concat arrays.
            (ty::Data::Array(lhs_elem), ty::Data::Array(rhs_elem)) => {
              let elem = st.get_ty(ty::Data::Union(BTreeSet::from([*lhs_elem, *rhs_elem])));
              st.get_ty(ty::Data::Array(elem))
            }
            // add object fields.
            (ty::Data::Object(lhs_obj), ty::Data::Object(rhs_obj)) => {
              let mut obj = lhs_obj.clone();
              // right overrides left.
              let rhs_known = rhs_obj.known.iter().map(|(k, &v)| (k.clone(), v));
              obj.known.extend(rhs_known);
              // this has unknown if either has unknown.
              obj.has_unknown = obj.has_unknown || rhs_obj.has_unknown;
              st.get_ty(ty::Data::Object(obj))
            }
            (ty::Data::Union(_), _) | (_, ty::Data::Union(_)) => {
              log::warn!("TODO: check unions for +");
              ty::Ty::ANY
            }
            _ => {
              st.err(expr, error::Kind::InvalidPlus(lhs_ty, rhs_ty));
              ty::Ty::ANY
            }
          }
        }
        BinaryOp::Mul
        | BinaryOp::Div
        | BinaryOp::Sub
        | BinaryOp::Shl
        | BinaryOp::Shr
        | BinaryOp::BitXor
        | BinaryOp::BitOr
        | BinaryOp::BitAnd => {
          st.unify(lhs.unwrap_or(expr), ty::Ty::NUMBER, lhs_ty);
          st.unify(rhs.unwrap_or(expr), ty::Ty::NUMBER, rhs_ty);
          ty::Ty::NUMBER
        }
        BinaryOp::Eq => ty::Ty::BOOL,
        BinaryOp::Lt | BinaryOp::LtEq | BinaryOp::Gt | BinaryOp::GtEq => {
          // TODO something about how the lhs_ty and rhs_ty need to be "similar" somehow (both
          // numbers or both strings, etc)
          if !is_orderable(st, lhs_ty) {
            st.err(lhs.unwrap_or(expr), error::Kind::Incomparable(lhs_ty));
          }
          if !is_orderable(st, rhs_ty) {
            st.err(rhs.unwrap_or(expr), error::Kind::Incomparable(rhs_ty));
          }
          ty::Ty::BOOL
        }
      }
    }
    ExprData::UnaryOp { inner, op } => {
      let inner_ty = get(st, ar, *inner);
      let e = inner.unwrap_or(expr);
      let want = match op {
        UnaryOp::Neg | UnaryOp::Pos | UnaryOp::BitNot => ty::Ty::NUMBER,
        UnaryOp::LogicalNot => ty::Ty::BOOL,
      };
      st.unify(e, want, inner_ty);
      want
    }
    ExprData::Error(inner) => {
      get(st, ar, *inner);
      ty::Ty::NEVER
    }
    ExprData::Import { kind, path } => match kind {
      jsonnet_expr::ImportKind::Code => {
        st.note_usage(expr, def::Def::Import(*path));
        st.import_ty(*path)
      }
      jsonnet_expr::ImportKind::String => ty::Ty::STRING,
      jsonnet_expr::ImportKind::Binary => ty::Ty::ARRAY_NUMBER,
    },
  };
  // NOTE: we CANNOT assert that this always return None. i'm pretty confident it's because of
  // duplication of expressions when lowering array/object comprehensions. i don't think that's a
  // huge problem.
  st.insert_expr_ty(expr, ret);
  ret
}

fn define_binds(
  st: &mut st::St<'_>,
  ar: &ExprArena,
  expr: jsonnet_expr::ExprMust,
  binds: &[(Id, jsonnet_expr::Expr)],
  m: def::ExprDefKindMulti,
) {
  let mut bound_ids = FxHashSet::<Id>::default();
  for (idx, &(bind, _)) in binds.iter().enumerate() {
    st.define(bind, ty::Ty::ANY, def::Def::Expr(expr, def::ExprDefKind::Multi(idx, m)));
    if !bound_ids.insert(bind) {
      st.err(expr, error::Kind::DuplicateBinding(bind, idx, m));
    }
  }
  for &(lhs, rhs) in binds {
    let ty = get(st, ar, rhs);
    always!(bound_ids.contains(&lhs), "should have just defined: {lhs:?}");
    st.refine(lhs, ty);
  }
}

fn is_orderable(st: &st::St<'_>, ty: ty::Ty) -> bool {
  match st.data(ty) {
    ty::Data::Prim(ty::Prim::Any | ty::Prim::Number | ty::Prim::String) => true,
    ty::Data::Prim(ty::Prim::True | ty::Prim::False | ty::Prim::Null)
    | ty::Data::Object(_)
    | ty::Data::Fn(_) => false,
    ty::Data::Array(ty) => is_orderable(st, *ty),
    ty::Data::Union(tys) => tys.iter().all(|&ty| is_orderable(st, ty)),
  }
}

/// collects facts from `asserts`s at a beginning of a fn, to refine the types of its params.
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
/// - cannot use e.g. `std.type(x) == "number"`
/// - cannot use `std.isFunction`, the type system cannot model a function with totally unknown
///   params. this wouldn't be that helpful anyway i suppose - if you don't know how many params,
///   how can you call it?
/// - cannot chain the asserts with `&&` or `||` - must put them one after the other
/// - asserts all be at the beginning of the fn, so cannot e.g. introduce new local variables
/// - cannot do `local isNumber = std.isNumber` beforehand, must literally get the field off `std`
/// - cannot use named arguments, only positional arguments
///
/// on the bright side:
///
/// - since asserts are lowered to `if cond then ... else error ...`, we check for that. so if the
///   user wrote that itself in the concrete syntax, that also works.
/// - checking we get from `std` is NOT syntactic, we do an env lookup. so we won't trick this by
///   doing `local std = wtf` beforehand, and also it'll still work with `local foo = std` and then
///   asserting with `foo.isTYPE` etc.
fn refine_param_tys(
  st: &st::St<'_>,
  ar: &ExprArena,
  params: &mut FxHashMap<Id, ty::Ty>,
  mut body: Expr,
) {
  while let Some(b) = body {
    let &ExprData::If { cond, yes, no: Some(no) } = &ar[b] else { break };
    let ExprData::Error(_) = &ar[no] else { break };
    body = yes;
    refine_param_ty_cond(st, ar, params, cond);
  }
}

/// refine from a single if-cond.
fn refine_param_ty_cond(
  st: &st::St<'_>,
  ar: &ExprArena,
  params: &mut FxHashMap<Id, ty::Ty>,
  cond: Expr,
) {
  let Some(cond) = cond else { return };
  let ExprData::Call { func: Some(func), positional, named } = &ar[cond] else { return };
  let &ExprData::Subscript { on: Some(on), idx: Some(idx) } = &ar[*func] else { return };
  let ExprData::Id(std_id) = &ar[on] else { return };
  if !st.is_std(*std_id) {
    return;
  }
  let ExprData::Prim(Prim::String(s)) = &ar[idx] else { return };
  let ty = match *s {
    jsonnet_expr::Str::isNumber => ty::Ty::NUMBER,
    jsonnet_expr::Str::isString => ty::Ty::STRING,
    jsonnet_expr::Str::isBoolean => ty::Ty::BOOL,
    jsonnet_expr::Str::isArray => ty::Ty::ARRAY_ANY,
    jsonnet_expr::Str::isObject => ty::Ty::OBJECT,
    _ => return,
  };
  if !named.is_empty() {
    return;
  }
  let [Some(param)] = positional[..] else { return };
  let &ExprData::Id(param) = &ar[param] else { return };
  let Some(param_ty) = params.get_mut(&param) else { return };
  if *param_ty == ty::Ty::ANY {
    *param_ty = ty;
  }
}
