//! Executing Jsonnet expression to produce Jsonnet values.

use crate::error::{self, Result};
use crate::{manifest, Cx};
use always::always;
use finite_float::Float;
use jsonnet_expr::{arg, BinOp, Expr, ExprData, ExprMust, Id, Prim, StdField, Str, StrArena};
use jsonnet_val::jsonnet::{
  Array, Env, ExprField, ExprFields, Field, Fn, Object, RegularFn, SelfRefer, Subst, Val, ValOrExpr,
};
use rustc_hash::FxHashSet;
use std::cmp::Ordering;

const EPSILON: f64 = 0.0001;

pub(crate) fn get(cx: &mut Cx<'_>, env: &Env, expr: Expr) -> Result<Val> {
  let Some(expr) = expr else { return Err(error::Error::NoExpr) };
  match cx.exprs[&env.path()].ar[expr].clone() {
    ExprData::Prim(p) => Ok(Val::Prim(p.clone())),
    ExprData::Object { asserts, fields } => {
      let mut named_fields = ExprFields::default();
      for field in fields {
        match get(cx, env, field.key)? {
          Val::Prim(Prim::String(s)) => {
            let f = ExprField { vis: field.vis, expr: field.val, comp_subst: None };
            if named_fields.insert(s, f).is_some() {
              return Err(error::Error::Exec { expr, kind: error::Kind::DuplicateField(s) });
            }
          }
          Val::Prim(Prim::Null) => {}
          _ => return Err(error::Error::Exec { expr, kind: error::Kind::IncompatibleTypes }),
        }
      }
      Ok(Val::Object(cx.obj_mk.mk(env.clone(), asserts.clone(), named_fields)))
    }
    ExprData::ObjectComp { name, vis, body, id, ary } => {
      let Val::Array(array) = get(cx, env, ary)? else {
        return Err(error::Error::Exec { expr, kind: error::Kind::IncompatibleTypes });
      };
      let mut fields = ExprFields::default();
      for (elem_env, elem_expr) in array.elems() {
        let mut env = env.clone();
        let subst = Subst { id, val: ValOrExpr::Expr(elem_env.clone(), elem_expr) };
        env.insert(subst);
        match get(cx, &env, name)? {
          Val::Prim(Prim::String(s)) => {
            let Some(body) = body else { continue };
            // the spec says to do `[e/x]body` here. we don't substitute eagerly, so instead we put
            // the e (elem) and x (id) on the object and update the env when we evaluate the field.
            let subst = Subst { id, val: ValOrExpr::Expr(elem_env.clone(), elem_expr) };
            let f = ExprField { vis, expr: Some(body), comp_subst: Some(subst) };
            if fields.insert(s, f).is_some() {
              return Err(error::Error::Exec { expr, kind: error::Kind::DuplicateField(s) });
            }
          }
          Val::Prim(Prim::Null) => {}
          _ => return Err(error::Error::Exec { expr, kind: error::Kind::IncompatibleTypes }),
        }
      }
      Ok(Val::Object(cx.obj_mk.mk(env.clone(), Vec::new(), fields)))
    }
    ExprData::Array(elems) => Ok(Val::Array(Array::new(env.clone(), elems.clone()))),
    ExprData::Subscript { on, idx } => match get(cx, env, on)? {
      Val::Object(object) => {
        let Val::Prim(Prim::String(name)) = get(cx, env, idx)? else {
          return Err(error::Error::Exec { expr, kind: error::Kind::IncompatibleTypes });
        };
        ck_object_asserts(cx, &object)?;
        let Some(field) = object.get_field(name) else {
          return Err(error::Error::Exec { expr, kind: error::Kind::NoSuchField(name) });
        };
        match field {
          Field::Std(field) => match field {
            StdField::thisFile => {
              let s = cx
                .paths
                .get_path(env.path())
                .as_path()
                .to_string_lossy()
                .into_owned()
                .into_boxed_str();
              Ok(Val::Prim(Prim::String(cx.str_ar.str(s))))
            }
            StdField::pi => Ok(Val::Prim(Prim::Number(Float::PI))),
            StdField::Fn(f) => Ok(Val::Fn(Fn::Std(f))),
          },
          Field::Expr(_, env, expr) => get(cx, &env, expr),
        }
      }
      Val::Array(array) => {
        let Val::Prim(Prim::Number(idx)) = get(cx, env, idx)? else {
          return Err(error::Error::Exec { expr, kind: error::Kind::IncompatibleTypes });
        };
        let idx = idx.value();
        let idx_floor = idx.floor();
        let diff = idx - idx_floor;
        if diff.abs() > EPSILON {
          return Err(error::Error::Exec { expr, kind: error::Kind::ArrayIdxNotInteger });
        }
        if idx_floor < 0.0 || idx_floor > f64::from(u32::MAX) {
          return Err(error::Error::Exec { expr, kind: error::Kind::ArrayIdxOutOfRange });
        }
        #[expect(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
        let idx = idx_floor as u32;
        let Ok(idx) = usize::try_from(idx) else {
          return Err(error::Error::Exec { expr, kind: error::Kind::ArrayIdxOutOfRange });
        };
        match array.get(idx) {
          Some((env, expr)) => get(cx, env, expr),
          None => Err(error::Error::Exec { expr, kind: error::Kind::ArrayIdxOutOfRange }),
        }
      }
      _ => Err(error::Error::Exec { expr, kind: error::Kind::IncompatibleTypes }),
    },
    ExprData::Call { func, positional, named } => {
      let Val::Fn(func) = get(cx, env, func)? else {
        return Err(error::Error::Exec { expr, kind: error::Kind::IncompatibleTypes });
      };
      get_call(cx, env, expr, func, &positional, &named)
    }
    ExprData::Id(id) => match env.get(id) {
      None => Err(error::Error::Exec { expr, kind: error::Kind::UndefinedVar(id) }),
      Some(got) => match got {
        ValOrExpr::Val(val) => Ok(val),
        ValOrExpr::Expr(env, e) => get(cx, &env, e),
      },
    },
    ExprData::Local { binds, body } => {
      let mut env = env.clone();
      env.add_binds(binds.clone(), SelfRefer::Yes);
      get(cx, &env, body)
    }
    ExprData::If { cond, yes, no } => {
      let Val::Prim(Prim::Bool(b)) = get(cx, env, cond)? else {
        return Err(error::Error::Exec { expr, kind: error::Kind::IncompatibleTypes });
      };
      let expr = if b { yes } else { no };
      get(cx, env, expr)
    }
    ExprData::BinOp { lhs, op, rhs } => match op {
      // add
      BinOp::Add => match (get(cx, env, lhs)?, get(cx, env, rhs)?) {
        (Val::Prim(Prim::String(lhs)), rhs) => {
          let rhs = str_conv(cx, rhs)?;
          Ok(Val::Prim(Prim::String(str_concat(cx.str_ar, lhs, rhs))))
        }
        (lhs, Val::Prim(Prim::String(rhs))) => {
          let lhs = str_conv(cx, lhs)?;
          Ok(Val::Prim(Prim::String(str_concat(cx.str_ar, lhs, rhs))))
        }
        (Val::Prim(Prim::Number(lhs)), Val::Prim(Prim::Number(rhs))) => {
          let n = match Float::try_from(lhs.value() + rhs.value()) {
            Ok(n) => n,
            Err(inf) => return Err(error::Error::Exec { expr, kind: error::Kind::Infinite(inf) }),
          };
          Ok(Val::Prim(Prim::Number(n)))
        }
        (Val::Array(mut lhs), Val::Array(mut rhs)) => {
          lhs.append(&mut rhs);
          Ok(Val::Array(lhs))
        }
        (Val::Object(lhs), Val::Object(mut rhs)) => {
          rhs.set_parent_to(lhs);
          Ok(Val::Object(rhs))
        }
        _ => Err(error::Error::Exec { expr, kind: error::Kind::IncompatibleTypes }),
      },
      // arithmetic
      BinOp::Mul => float_op(expr, cx, env, lhs, rhs, std::ops::Mul::mul),
      BinOp::Div => float_op(expr, cx, env, lhs, rhs, std::ops::Div::div),
      BinOp::Sub => float_op(expr, cx, env, lhs, rhs, std::ops::Sub::sub),
      // bitwise
      BinOp::Shl => int_op(expr, cx, env, lhs, rhs, std::ops::Shl::shl),
      BinOp::Shr => int_op(expr, cx, env, lhs, rhs, std::ops::Shr::shr),
      BinOp::BitAnd => int_op(expr, cx, env, lhs, rhs, std::ops::BitAnd::bitand),
      BinOp::BitXor => int_op(expr, cx, env, lhs, rhs, std::ops::BitXor::bitxor),
      BinOp::BitOr => int_op(expr, cx, env, lhs, rhs, std::ops::BitOr::bitor),
      // comparison
      BinOp::Eq => {
        let lhs = get(cx, env, lhs)?;
        let rhs = get(cx, env, rhs)?;
        Ok(Val::Prim(Prim::Bool(eq_val(expr, cx, &lhs, &rhs)?)))
      }
      BinOp::Lt => cmp_bool_op(expr, cx, env, lhs, rhs, Ordering::is_lt),
      BinOp::LtEq => cmp_bool_op(expr, cx, env, lhs, rhs, Ordering::is_le),
      BinOp::Gt => cmp_bool_op(expr, cx, env, lhs, rhs, Ordering::is_gt),
      BinOp::GtEq => cmp_bool_op(expr, cx, env, lhs, rhs, Ordering::is_ge),
    },
    ExprData::UnOp { op, inner } => {
      let inner = get(cx, env, inner)?;
      match op {
        jsonnet_expr::UnOp::Neg => {
          if let Val::Prim(Prim::Number(n)) = inner {
            Ok(Val::Prim(Prim::Number(-n)))
          } else {
            Err(error::Error::Exec { expr, kind: error::Kind::IncompatibleTypes })
          }
        }
        jsonnet_expr::UnOp::Pos => {
          if matches!(inner, Val::Prim(Prim::Number(_))) {
            Ok(inner)
          } else {
            Err(error::Error::Exec { expr, kind: error::Kind::IncompatibleTypes })
          }
        }
        jsonnet_expr::UnOp::LogicalNot => {
          if let Val::Prim(Prim::Bool(b)) = inner {
            Ok(Val::Prim(Prim::Bool(!b)))
          } else {
            Err(error::Error::Exec { expr, kind: error::Kind::IncompatibleTypes })
          }
        }
        jsonnet_expr::UnOp::BitNot => {
          if let Val::Prim(Prim::Number(n)) = inner {
            let n = n.value().round();
            #[expect(clippy::cast_precision_loss)]
            let n = n.clamp(i64::MIN as f64, i64::MAX as f64);
            #[expect(clippy::cast_possible_truncation)]
            let n = n as i64;
            let n = !n;
            #[expect(clippy::cast_precision_loss)]
            let n = n as f64;
            let n = Float::always_from_f64(n);
            Ok(Val::Prim(Prim::Number(n)))
          } else {
            Err(error::Error::Exec { expr, kind: error::Kind::IncompatibleTypes })
          }
        }
      }
    }
    ExprData::Fn { params, body } => {
      Ok(Val::Fn(Fn::Regular(RegularFn { env: env.clone(), params: params.clone(), body })))
    }
    ExprData::Error(inner) => {
      let val = get(cx, env, inner)?;
      let msg = str_conv(cx, val)?;
      Err(error::Error::Exec { expr, kind: error::Kind::User(msg) })
    }
    ExprData::Import { kind, path } => match kind {
      jsonnet_expr::ImportKind::Code => match cx.exprs.get(&path) {
        Some(file) => match env.empty_with_paths(path) {
          Ok(env) => get(cx, &env, file.top),
          Err(cycle) => Err(error::Error::Exec { expr, kind: error::Kind::Cycle(cycle) }),
        },
        None => Err(error::Error::NoPath(path)),
      },
      jsonnet_expr::ImportKind::String => match cx.import_str.get(&path) {
        Some(s) => Ok(Val::Prim(Prim::String(cx.str_ar.str(s.clone().into_boxed_str())))),
        None => Err(error::Error::NoPath(path)),
      },
      jsonnet_expr::ImportKind::Binary => match cx.import_bin.get(&path) {
        Some(bs) => {
          let Some(exprs) = cx.exprs.get_mut(&env.path()) else {
            always!(false, "should have this paths's exprs");
            return Err(error::Error::NoPath(env.path()));
          };
          let elems = bs.iter().map(|&n| {
            let ed = ExprData::Prim(Prim::Number(Float::from(n)));
            Some(exprs.ar.alloc(ed))
          });
          Ok(Array::new(Env::empty(env.path()), elems.collect()).into())
        }
        None => Err(error::Error::NoPath(path)),
      },
    },
    ExprData::SubstOuter(expr) => {
      let mut env = env.clone();
      env.use_outer_self_super();
      get(cx, &env, expr)
    }
  }
}

pub(crate) fn get_call(
  cx: &mut Cx<'_>,
  env: &Env,
  expr: ExprMust,
  func: Fn,
  pos: &[Expr],
  named: &[(Id, Expr)],
) -> Result<Val> {
  match func {
    Fn::Regular(func) => get_regular_call(cx, env, expr, func, pos, named),
    Fn::Std(func) => crate::std_lib::get_call(cx, env, expr, func, pos, named),
  }
}

fn get_regular_call(
  cx: &mut Cx<'_>,
  env: &Env,
  expr: ExprMust,
  mut func: RegularFn,
  positional: &[Expr],
  named: &[(Id, Expr)],
) -> Result<Val> {
  if let Some(tma) = arg::TooMany::new(
    func.params.iter().map(|&(id, _)| id),
    positional.len(),
    named.iter().map(|&(id, _)| id),
  ) {
    return Err(error::Error::Exec { expr, kind: arg::ErrorKind::TooMany(tma).into() });
  }
  let mut provided = FxHashSet::<Id>::default();
  for ((id, param), &arg) in func.params.iter_mut().zip(positional) {
    *param = Some(arg);
    always!(provided.insert(*id), "duplicate function param should be forbidden by check");
  }
  for &(arg_name, arg) in named {
    if !provided.insert(arg_name) {
      return Err(error::Error::Exec {
        expr: arg.unwrap_or(expr),
        kind: arg::ErrorKind::Duplicate(arg_name).into(),
      });
    }
    // we're getting a little fancy here. this iterates across the mutable params, and if we could
    // find a param whose name matches the arg's name, then this sets the param to that arg and
    // short circuits with true. note `==` with comparing the names and `=` with setting the actual
    // exprs. note the usage of `bool::then` with `find_map` and `is_none`.
    let arg_not_requested = func
      .params
      .iter_mut()
      .find_map(|(param_name, param)| (*param_name == arg_name).then(|| *param = Some(arg)))
      .is_none();
    if arg_not_requested {
      return Err(error::Error::Exec {
        expr: arg.unwrap_or(expr),
        kind: arg::ErrorKind::NotRequested(arg_name).into(),
      });
    }
  }
  let mut provided_binds = Vec::<(Id, Expr)>::with_capacity(provided.len());
  let mut default_binds = Vec::<(Id, Expr)>::new();
  for (id, rhs) in func.params {
    // from my (not super close) reading of the spec, it seems like for function parameters without
    // default values, the default value should be set to `error "Parameter not bound"`, which will
    // _lazily_ emit the error if the parameter is accessed. but the behavior of the impl on the
    // website is to _eagerly_ error if a param is not defined. so we do that here.
    let Some(rhs) = rhs else {
      return Err(error::Error::Exec { expr, kind: arg::ErrorKind::NotDefined(id).into() });
    };
    let binds = if provided.contains(&id) { &mut provided_binds } else { &mut default_binds };
    binds.push((id, rhs));
  }
  // first remove the provided binds from the func env, so we don't accidentally shadow them when we
  // add the func env so we can evaluate the default binds.
  let mut func_env = func.env.clone();
  for &(id, _) in &provided_binds {
    func_env.remove(id);
  }
  // start with the current env.
  let mut env = env.clone();
  // add the provided binds to that. we evaluate the provided binds under the current env.
  //
  // these binds do NOT recursively refer to themselves. for example, if we have
  //
  // local f(x) = local g(x) = x + 1; g(x)
  //
  // then the x passed to the call to g refers to the param of f, NOT the param of g.
  env.add_binds(provided_binds, SelfRefer::No);
  // then append the func env onto the current env.
  //
  // if the func default params or body mention names that are defined in both the current env and
  // the func env, they will be shadowed by the func env, except for the provided binds which we
  // already removed.
  //
  // if the func default params or body mention names that are only in the func env, they will be
  // defined by the func env.
  //
  // the func body cannot mention names that are only in the current env but not the fn's env, since
  // this is prohibited by statics.
  env.append(&mut func_env);
  // finally add the default binds. these are the arguments that were not provided and will default
  // to the default argument values, to be evaluated under the func env.
  //
  // these binds MAY refer to themselves. for example, in
  //
  // local f(a=2, b=a) = a + b
  //
  // we have f() == 4.
  env.add_binds(default_binds, SelfRefer::Yes);
  // now evaluate the body.
  get(cx, &env, func.body)
}

fn number_pair(expr: ExprMust, cx: &mut Cx<'_>, env: &Env, a: Expr, b: Expr) -> Result<[Float; 2]> {
  match (get(cx, env, a)?, get(cx, env, b)?) {
    (Val::Prim(Prim::Number(a)), Val::Prim(Prim::Number(b))) => Ok([a, b]),
    _ => Err(error::Error::Exec { expr, kind: error::Kind::IncompatibleTypes }),
  }
}

fn float_op<F>(
  expr: ExprMust,
  cx: &mut Cx<'_>,
  env: &Env,
  lhs: Expr,
  rhs: Expr,
  f: F,
) -> Result<Val>
where
  F: FnOnce(f64, f64) -> f64,
{
  let [lhs, rhs] = number_pair(expr, cx, env, lhs, rhs)?;
  let n = match Float::try_from(f(lhs.value(), rhs.value())) {
    Ok(n) => n,
    Err(inf) => return Err(error::Error::Exec { expr, kind: error::Kind::Infinite(inf) }),
  };
  Ok(Val::Prim(Prim::Number(n)))
}

fn int_op<F>(expr: ExprMust, cx: &mut Cx<'_>, env: &Env, lhs: Expr, rhs: Expr, f: F) -> Result<Val>
where
  F: FnOnce(i64, i64) -> i64,
{
  let ns = number_pair(expr, cx, env, lhs, rhs)?;
  #[expect(clippy::cast_possible_truncation)]
  let [lhs, rhs] = ns.map(|x| x.value() as i64);
  #[expect(clippy::cast_precision_loss)]
  let n = f(lhs, rhs) as f64;
  let n = match Float::try_from(n) {
    Ok(n) => n,
    Err(inf) => return Err(error::Error::Exec { expr, kind: error::Kind::Infinite(inf) }),
  };
  Ok(Val::Prim(Prim::Number(n)))
}

pub(crate) fn cmp_op<F>(
  expr: ExprMust,
  cx: &mut Cx<'_>,
  env: &Env,
  lhs: Expr,
  rhs: Expr,
  f: F,
) -> Result<Val>
where
  F: FnOnce(Ordering) -> Prim,
{
  let lhs = get(cx, env, lhs)?;
  let rhs = get(cx, env, rhs)?;
  let ord = cmp_val(expr, cx, &lhs, &rhs)?;
  Ok(Val::Prim(f(ord)))
}

fn cmp_bool_op<F>(
  expr: ExprMust,
  cx: &mut Cx<'_>,
  env: &Env,
  lhs: Expr,
  rhs: Expr,
  f: F,
) -> Result<Val>
where
  F: FnOnce(Ordering) -> bool,
{
  cmp_op(expr, cx, env, lhs, rhs, |x| Prim::Bool(f(x)))
}

fn cmp_val(expr: ExprMust, cx: &mut Cx<'_>, lhs: &Val, rhs: &Val) -> Result<Ordering> {
  match (lhs, rhs) {
    (Val::Prim(lhs), Val::Prim(rhs)) => match (lhs, rhs) {
      (Prim::String(lhs), Prim::String(rhs)) => Ok(cx.str_ar.get(*lhs).cmp(cx.str_ar.get(*rhs))),
      (Prim::Number(lhs), Prim::Number(rhs)) => Ok(lhs.cmp(rhs)),
      _ => Err(error::Error::Exec { expr, kind: error::Kind::IncompatibleTypes }),
    },
    (Val::Array(lhs), Val::Array(rhs)) => {
      let lhs = lhs.elems();
      let rhs = rhs.elems();
      let mut lhs = lhs.into_iter();
      let mut rhs = rhs.into_iter();
      let ord = loop {
        match (lhs.next(), rhs.next()) {
          (None, Some(_)) => break Ordering::Less,
          (None, None) => break Ordering::Equal,
          (Some(_), None) => break Ordering::Greater,
          (Some((lhs_env, lhs_elem)), Some((rhs_env, rhs_elem))) => {
            let lhs = get(cx, lhs_env, lhs_elem)?;
            let rhs = get(cx, rhs_env, rhs_elem)?;
            match cmp_val(expr, cx, &lhs, &rhs)? {
              Ordering::Equal => {}
              ord => break ord,
            }
          }
        }
      };
      Ok(ord)
    }
    _ => Err(error::Error::Exec { expr, kind: error::Kind::IncompatibleTypes }),
  }
}

pub(crate) fn eq_val(expr: ExprMust, cx: &mut Cx<'_>, lhs: &Val, rhs: &Val) -> Result<bool> {
  match (lhs, rhs) {
    (Val::Prim(lhs), Val::Prim(rhs)) => Ok(lhs == rhs),
    (Val::Array(lhs), Val::Array(rhs)) => {
      if lhs.len() != rhs.len() {
        return Ok(false);
      }
      let lhs = lhs.elems();
      let rhs = rhs.elems();
      for ((lhs_env, lhs_elem), (rhs_env, rhs_elem)) in lhs.into_iter().zip(rhs) {
        let lhs = get(cx, lhs_env, lhs_elem)?;
        let rhs = get(cx, rhs_env, rhs_elem)?;
        let eq = eq_val(expr, cx, &lhs, &rhs)?;
        if !eq {
          return Ok(false);
        }
      }
      Ok(true)
    }
    (Val::Object(lhs), Val::Object(rhs)) => {
      ck_object_asserts(cx, lhs)?;
      ck_object_asserts(cx, rhs)?;
      let lhs_fields = lhs.sorted_visible_fields();
      let rhs_fields = rhs.sorted_visible_fields();
      if lhs_fields.len() != rhs_fields.len() {
        return Ok(false);
      }
      for (lhs_field, rhs_field) in lhs_fields.into_iter().zip(rhs_fields) {
        let (lhs_name, lhs_env, lhs_expr) = lhs_field;
        let (rhs_name, rhs_env, rhs_expr) = rhs_field;
        if lhs_name != rhs_name {
          return Ok(false);
        }
        let lhs = get(cx, &lhs_env, lhs_expr)?;
        let rhs = get(cx, &rhs_env, rhs_expr)?;
        let eq = eq_val(expr, cx, &lhs, &rhs)?;
        if !eq {
          return Ok(false);
        }
      }
      Ok(true)
    }
    (Val::Fn(_), Val::Fn(_)) => Err(error::Error::Exec { expr, kind: error::Kind::EqFn }),
    _ => Ok(false),
  }
}

fn str_conv(cx: &mut Cx<'_>, val: Val) -> Result<Str> {
  if let Val::Prim(Prim::String(s)) = val {
    Ok(s)
  } else {
    let json = manifest::get(cx, val)?;
    let string = json.display(cx.str_ar).to_string();
    Ok(cx.str_ar.str(string.into_boxed_str()))
  }
}

fn str_concat(ar: &mut StrArena, lhs: Str, rhs: Str) -> Str {
  let lhs = ar.get(lhs);
  let rhs = ar.get(rhs);
  let both = format!("{lhs}{rhs}").into_boxed_str();
  ar.str(both)
}

pub(crate) fn ck_object_asserts(cx: &mut Cx<'_>, object: &Object) -> Result<()> {
  if !cx.obj_mk.start_checking_asserts(object) {
    return Ok(());
  }
  for (env, assert) in object.asserts() {
    let v = get(cx, &env, assert)?;
    always!(matches!(v, Val::Prim(Prim::Null)), "bad desugar for assert, got non-Null");
  }
  cx.obj_mk.finish_checking_asserts(object);
  Ok(())
}
