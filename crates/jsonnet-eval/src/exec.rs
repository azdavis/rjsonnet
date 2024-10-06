//! Executing Jsonnet expression to produce Jsonnet values.

use crate::error::{self, Result};
use crate::val::jsonnet::{Array, Env, Field, Fn, Get, Object, RegularFn, Val};
use crate::{manifest, mk_todo, std_lib, Cx};
use always::always;
use finite_float::Float;
use jsonnet_expr::{
  arg, BinaryOp, Expr, ExprData, ExprMust, Id, Prim, StdField, Str, StrArena, Visibility,
};
use rustc_hash::FxHashSet;
use std::cmp::Ordering;
use std::collections::BTreeMap;

const EPSILON: f64 = 0.0001;

pub(crate) fn get(cx: Cx<'_>, env: &Env, expr: Expr) -> Result<Val> {
  let Some(expr) = expr else { return Err(error::Error::NoExpr) };
  let expr_ar = &cx.exprs[&env.path].ar;
  match &expr_ar[expr] {
    ExprData::Prim(p) => Ok(Val::Prim(p.clone())),
    ExprData::Object { binds, asserts, fields } => {
      let mut named_fields = BTreeMap::<Str, (Visibility, Expr)>::default();
      for field in fields {
        match get(cx, env, field.key)? {
          Val::Prim(Prim::String(s)) => {
            if named_fields.insert(s.clone(), (field.vis, field.val)).is_some() {
              return Err(error::Error::Exec { expr, kind: error::Kind::DuplicateField(s) });
            }
          }
          Val::Prim(Prim::Null) => {}
          _ => return Err(error::Error::Exec { expr, kind: error::Kind::IncompatibleTypes }),
        }
      }
      Ok(Val::Object(Object::new(env.clone(), binds.clone(), asserts.clone(), named_fields)))
    }
    ExprData::ObjectComp { name, body, id, ary } => {
      let Val::Array(array) = get(cx, env, *ary)? else {
        return Err(error::Error::Exec { expr, kind: error::Kind::IncompatibleTypes });
      };
      let mut fields = BTreeMap::<Str, (Visibility, Expr)>::default();
      for (part_env, elem) in array.iter() {
        let mut env = env.clone();
        env.insert(*id, part_env.clone(), elem);
        match get(cx, &env, *name)? {
          Val::Prim(Prim::String(s)) => {
            let Some(body) = *body else { continue };
            // TODO the spec says to do `[e/x]body` here. we'd rather not substitute eagerly. we
            // should do something like put the e and x on the object literal and update the env
            // when we would evaluate the field.
            let body = match expr_ar[body] {
              ExprData::Prim(_) => body,
              _ => return Err(mk_todo(expr, "subst for object body")),
            };
            if fields.insert(s.clone(), (Visibility::Default, Some(body))).is_some() {
              return Err(error::Error::Exec { expr, kind: error::Kind::DuplicateField(s) });
            }
          }
          Val::Prim(Prim::Null) => {}
          _ => return Err(error::Error::Exec { expr, kind: error::Kind::IncompatibleTypes }),
        }
      }
      Ok(Val::Object(Object::new(env.clone(), Vec::new(), Vec::new(), fields)))
    }
    ExprData::Array(elems) => Ok(Val::Array(Array::new(env.clone(), elems.clone()))),
    ExprData::Subscript { on, idx } => match get(cx, env, *on)? {
      Val::Object(object) => {
        let Val::Prim(Prim::String(name)) = get(cx, env, *idx)? else {
          return Err(error::Error::Exec { expr, kind: error::Kind::IncompatibleTypes });
        };
        let Some((_, field)) = object.get_field(&name) else {
          return Err(error::Error::Exec {
            expr,
            kind: error::Kind::FieldNotDefined(name.clone()),
          });
        };
        for (env, assert) in object.asserts() {
          get(cx, &env, assert)?;
        }
        match field {
          Field::Std(field) => match field {
            StdField::ThisFile => {
              let s = cx
                .paths
                .get_path(env.path)
                .as_path()
                .to_string_lossy()
                .into_owned()
                .into_boxed_str();
              Ok(Val::Prim(Prim::String(cx.str_ar.str_shared(s))))
            }
            StdField::Fn(f) => Ok(Val::Fn(Fn::Std(f))),
          },
          Field::Expr(env, expr) => get(cx, &env, expr),
        }
      }
      Val::Array(array) => {
        let Val::Prim(Prim::Number(idx)) = get(cx, env, *idx)? else {
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
          Some((env, elem)) => get(cx, env, elem),
          None => Err(error::Error::Exec { expr, kind: error::Kind::ArrayIdxOutOfRange }),
        }
      }
      _ => Err(error::Error::Exec { expr, kind: error::Kind::IncompatibleTypes }),
    },
    ExprData::Call { func, positional, named } => {
      let func = get(cx, env, *func)?;
      get_call(cx, env, expr, func, positional, named)
    }
    ExprData::Id(id) => match env.get(*id) {
      None => Err(error::Error::Exec { expr, kind: error::Kind::NotInScope(*id) }),
      Some(got) => match got {
        Get::Self_ => match env.this() {
          Some(this) => Ok(Val::Object(this.clone())),
          None => Err(error::Error::Exec { expr, kind: error::Kind::NotInScope(*id) }),
        },
        Get::Super => match env.this() {
          Some(this) => Ok(Val::Object(this.parent())),
          None => Err(error::Error::Exec { expr, kind: error::Kind::NotInScope(*id) }),
        },
        Get::Std => Ok(Val::Object(Object::std_lib())),
        Get::Expr(env, expr) => get(cx, env, expr),
      },
    },
    ExprData::Local { binds, body } => get(cx, &env.add_binds(binds), *body),
    ExprData::If { cond, yes, no } => {
      let Val::Prim(Prim::Bool(b)) = get(cx, env, *cond)? else {
        return Err(error::Error::Exec { expr, kind: error::Kind::IncompatibleTypes });
      };
      let &expr = if b { yes } else { no };
      get(cx, env, expr)
    }
    ExprData::BinaryOp { lhs, op, rhs } => match op {
      // add
      BinaryOp::Add => match (get(cx, env, *lhs)?, get(cx, env, *rhs)?) {
        (Val::Prim(Prim::String(lhs)), rhs) => {
          let rhs = str_conv(cx, rhs)?;
          Ok(Val::Prim(Prim::String(str_concat(cx.str_ar, &lhs, &rhs))))
        }
        (lhs, Val::Prim(Prim::String(rhs))) => {
          let lhs = str_conv(cx, lhs)?;
          Ok(Val::Prim(Prim::String(str_concat(cx.str_ar, &lhs, &rhs))))
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
      BinaryOp::Mul => float_op(expr, cx, env, *lhs, *rhs, std::ops::Mul::mul),
      BinaryOp::Div => float_op(expr, cx, env, *lhs, *rhs, std::ops::Div::div),
      BinaryOp::Sub => float_op(expr, cx, env, *lhs, *rhs, std::ops::Sub::sub),
      // bitwise
      BinaryOp::Shl => int_op(expr, cx, env, *lhs, *rhs, std::ops::Shl::shl),
      BinaryOp::Shr => int_op(expr, cx, env, *lhs, *rhs, std::ops::Shr::shr),
      BinaryOp::BitAnd => int_op(expr, cx, env, *lhs, *rhs, std::ops::BitAnd::bitand),
      BinaryOp::BitXor => int_op(expr, cx, env, *lhs, *rhs, std::ops::BitXor::bitxor),
      BinaryOp::BitOr => int_op(expr, cx, env, *lhs, *rhs, std::ops::BitOr::bitor),
      // comparison
      BinaryOp::Eq => {
        let lhs = get(cx, env, *lhs)?;
        let rhs = get(cx, env, *rhs)?;
        Ok(Val::Prim(Prim::Bool(eq_val(expr, cx, &lhs, &rhs)?)))
      }
      BinaryOp::Lt => cmp_bool_op(expr, cx, env, *lhs, *rhs, Ordering::is_lt),
      BinaryOp::LtEq => cmp_bool_op(expr, cx, env, *lhs, *rhs, Ordering::is_le),
      BinaryOp::Gt => cmp_bool_op(expr, cx, env, *lhs, *rhs, Ordering::is_gt),
      BinaryOp::GtEq => cmp_bool_op(expr, cx, env, *lhs, *rhs, Ordering::is_ge),
    },
    ExprData::UnaryOp { op, inner } => {
      let inner = get(cx, env, *inner)?;
      match op {
        jsonnet_expr::UnaryOp::Neg => {
          if let Val::Prim(Prim::Number(n)) = inner {
            Ok(Val::Prim(Prim::Number(-n)))
          } else {
            Err(error::Error::Exec { expr, kind: error::Kind::IncompatibleTypes })
          }
        }
        jsonnet_expr::UnaryOp::Pos => {
          if matches!(inner, Val::Prim(Prim::Number(_))) {
            Ok(inner)
          } else {
            Err(error::Error::Exec { expr, kind: error::Kind::IncompatibleTypes })
          }
        }
        jsonnet_expr::UnaryOp::LogicalNot => {
          if let Val::Prim(Prim::Bool(b)) = inner {
            Ok(Val::Prim(Prim::Bool(!b)))
          } else {
            Err(error::Error::Exec { expr, kind: error::Kind::IncompatibleTypes })
          }
        }
        jsonnet_expr::UnaryOp::BitNot => {
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
    ExprData::Function { params, body } => {
      Ok(Val::Fn(Fn::Regular(RegularFn { env: env.clone(), params: params.clone(), body: *body })))
    }
    ExprData::Error(inner) => {
      let val = get(cx, env, *inner)?;
      let msg = str_conv(cx, val)?;
      Err(error::Error::Exec { expr, kind: error::Kind::User(msg) })
    }
    ExprData::Import { kind, path } => match kind {
      jsonnet_expr::ImportKind::Code => match cx.exprs.get(path) {
        Some(file) => match env.empty_with_cur(*path) {
          Ok(env) => get(cx, &env, file.top),
          Err(cycle) => Err(error::Error::Exec { expr, kind: error::Kind::Cycle(cycle) }),
        },
        None => Err(mk_todo(expr, "no code import")),
      },
      jsonnet_expr::ImportKind::String => match cx.import_str.get(path) {
        Some(s) => Ok(Val::Prim(Prim::String(cx.str_ar.str_shared(s.clone().into_boxed_str())))),
        None => Err(mk_todo(expr, "no string import")),
      },
      jsonnet_expr::ImportKind::Binary => match cx.import_bin.get(path) {
        Some(_) => Err(mk_todo(expr, "yes binary import")),
        None => Err(mk_todo(expr, "no binary import")),
      },
    },
  }
}

pub(crate) fn get_call(
  cx: Cx<'_>,
  env: &Env,
  expr: ExprMust,
  func_val: Val,
  positional: &[Expr],
  named: &[(Id, Expr)],
) -> Result<Val> {
  match func_val {
    Val::Fn(Fn::Regular(mut func)) => {
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
        // we're getting a little fancy here. this iterates across the mutable params, and if we
        // could find a param whose name matches the arg's name, then this sets the param to that
        // arg and short circuits with true. note `==` with comparing the names and `=` with setting
        // the actual exprs. note the usage of `bool::then` with `find_map` and `is_none`.
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
      let mut env = func.env.clone();
      for (id, rhs) in func.params {
        match rhs {
          // from my (not super close) reading of the spec, it seems like for function parameters
          // without default values, the default value should be set to `error "Parameter not
          // bound"`, which will _lazily_ emit the error if the parameter is accessed. but the
          // behavior of the impl on the website is to _eagerly_ error if a param is not defined. so
          // we do that here.
          None => {
            return Err(error::Error::Exec { expr, kind: arg::ErrorKind::NotDefined(id).into() })
          }
          Some(rhs) => env.insert(id, func.env.clone(), rhs),
        }
      }
      get(cx, &env, func.body)
    }
    Val::Fn(Fn::Std(std_fn)) => std_lib::get(cx, env, positional, named, expr, std_fn),
    _ => Err(error::Error::Exec { expr, kind: error::Kind::IncompatibleTypes }),
  }
}

fn number_pair(expr: ExprMust, cx: Cx<'_>, env: &Env, a: Expr, b: Expr) -> Result<[Float; 2]> {
  match (get(cx, env, a)?, get(cx, env, b)?) {
    (Val::Prim(Prim::Number(a)), Val::Prim(Prim::Number(b))) => Ok([a, b]),
    _ => Err(error::Error::Exec { expr, kind: error::Kind::IncompatibleTypes }),
  }
}

fn float_op<F>(expr: ExprMust, cx: Cx<'_>, env: &Env, lhs: Expr, rhs: Expr, f: F) -> Result<Val>
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

fn int_op<F>(expr: ExprMust, cx: Cx<'_>, env: &Env, lhs: Expr, rhs: Expr, f: F) -> Result<Val>
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
  cx: Cx<'_>,
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

fn cmp_bool_op<F>(expr: ExprMust, cx: Cx<'_>, env: &Env, lhs: Expr, rhs: Expr, f: F) -> Result<Val>
where
  F: FnOnce(Ordering) -> bool,
{
  cmp_op(expr, cx, env, lhs, rhs, |x| Prim::Bool(f(x)))
}

fn cmp_val(expr: ExprMust, cx: Cx<'_>, lhs: &Val, rhs: &Val) -> Result<Ordering> {
  match (lhs, rhs) {
    (Val::Prim(lhs), Val::Prim(rhs)) => match (lhs, rhs) {
      (Prim::String(lhs), Prim::String(rhs)) => Ok(cx.str_ar.get(lhs).cmp(cx.str_ar.get(rhs))),
      (Prim::Number(lhs), Prim::Number(rhs)) => Ok(lhs.cmp(rhs)),
      _ => Err(error::Error::Exec { expr, kind: error::Kind::IncompatibleTypes }),
    },
    (Val::Array(lhs), Val::Array(rhs)) => {
      let mut lhs = lhs.iter();
      let mut rhs = rhs.iter();
      let ord = loop {
        match (lhs.next(), rhs.next()) {
          (None, Some(_)) => break Ordering::Less,
          (None, None) => break Ordering::Equal,
          (Some(_), None) => break Ordering::Greater,
          (Some((le, lhs)), Some((re, rhs))) => {
            let lhs = get(cx, le, lhs)?;
            let rhs = get(cx, re, rhs)?;
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

/// TODO could this call manifest?
pub(crate) fn eq_val(expr: ExprMust, cx: Cx<'_>, lhs: &Val, rhs: &Val) -> Result<bool> {
  match (lhs, rhs) {
    (Val::Prim(lhs), Val::Prim(rhs)) => Ok(lhs == rhs),
    (Val::Array(lhs), Val::Array(rhs)) => {
      if lhs.len() != rhs.len() {
        return Ok(false);
      }
      for ((le, lhs), (re, rhs)) in lhs.iter().zip(rhs.iter()) {
        let lhs = get(cx, le, lhs)?;
        let rhs = get(cx, re, rhs)?;
        let eq = eq_val(expr, cx, &lhs, &rhs)?;
        if !eq {
          return Ok(false);
        }
      }
      Ok(true)
    }
    (Val::Object(_), Val::Object(_)) => {
      Err(error::Error::Exec { expr, kind: error::Kind::Todo("object-object equality") })
    }
    (Val::Prim(_), Val::Object(_) | Val::Array(_))
    | (Val::Object(_), Val::Prim(_) | Val::Array(_))
    | (Val::Array(_), Val::Prim(_) | Val::Object(_))
    | (Val::Fn(_), _)
    | (_, Val::Fn(_)) => Ok(false),
  }
}

fn str_conv(cx: Cx<'_>, val: Val) -> Result<Str> {
  if let Val::Prim(Prim::String(s)) = val {
    Ok(s)
  } else {
    let json = manifest::get(cx, val)?;
    let string = json.display(cx.str_ar, 0).to_string();
    Ok(cx.str_ar.str_shared(string.into_boxed_str()))
  }
}

fn str_concat(ar: &StrArena, lhs: &Str, rhs: &Str) -> Str {
  let lhs = ar.get(lhs);
  let rhs = ar.get(rhs);
  let both = format!("{lhs}{rhs}").into_boxed_str();
  ar.str_shared(both)
}
