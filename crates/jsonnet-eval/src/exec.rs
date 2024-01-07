//! Executing Jsonnet expression to produce Jsonnet values.

use crate::error::{self, Result};
use crate::manifest;
use crate::val::jsonnet::{Array, Env, Field, Get, Object, StdField, Val};
use jsonnet_expr::{
  arg, std_fn, Arenas, BinaryOp, Expr, ExprData, ExprMust, Id, Number, Prim, StdFn, Str, StrArena,
  Visibility,
};
use rustc_hash::FxHashSet;
use std::cmp::Ordering;
use std::collections::BTreeMap;

const EPSILON: f64 = 0.0001;

/// Executes the Jsonnet expression to produce a Jsonnet value.
///
/// # Errors
///
/// If execution failed.
///
/// # Panics
///
/// If the expr wasn't checked.
pub fn get(env: &Env, ars: &Arenas, expr: Expr) -> Result<Val> {
  let expr = expr.expect("no expr");
  match &ars.expr[expr] {
    ExprData::Prim(p) => Ok(Val::Prim(p.clone())),
    ExprData::Object { asserts, fields } => {
      let mut named_fields = BTreeMap::<Str, (Visibility, Expr)>::default();
      for &(key, hid, val) in fields {
        match get(env, ars, key)? {
          Val::Prim(Prim::String(s)) => {
            if named_fields.insert(s.clone(), (hid, val)).is_some() {
              return Err(error::Error::Exec { expr, kind: error::Kind::DuplicateField(s) });
            }
          }
          Val::Prim(Prim::Null) => {}
          _ => return Err(error::Error::Exec { expr, kind: error::Kind::IncompatibleTypes }),
        }
      }
      Ok(Val::Object(Object::new(env.clone(), asserts.clone(), named_fields)))
    }
    ExprData::ObjectComp { name, body, id, ary } => {
      let Val::Array(array) = get(env, ars, *ary)? else {
        return Err(error::Error::Exec { expr, kind: error::Kind::IncompatibleTypes });
      };
      let mut fields = BTreeMap::<Str, (Visibility, Expr)>::default();
      for (part_env, elem) in array.iter() {
        let mut env = env.clone();
        env.insert(*id, part_env.clone(), elem);
        match get(&env, ars, *name)? {
          Val::Prim(Prim::String(s)) => {
            let Some(body) = *body else { continue };
            // we want to do `[e/x]body` here?
            let body = match ars.expr[body] {
              ExprData::Prim(_) => body,
              _ => todo!("subst for object comp"),
            };
            if fields.insert(s.clone(), (Visibility::Default, Some(body))).is_some() {
              return Err(error::Error::Exec { expr, kind: error::Kind::DuplicateField(s) });
            }
          }
          Val::Prim(Prim::Null) => {}
          _ => return Err(error::Error::Exec { expr, kind: error::Kind::IncompatibleTypes }),
        }
      }
      Ok(Val::Object(Object::new(env.clone(), Vec::new(), fields)))
    }
    ExprData::Array(elems) => Ok(Val::Array(Array::new(env.clone(), elems.clone()))),
    ExprData::Subscript { on, idx } => match get(env, ars, *on)? {
      Val::Object(object) => {
        let Val::Prim(Prim::String(name)) = get(env, ars, *idx)? else {
          return Err(error::Error::Exec { expr, kind: error::Kind::IncompatibleTypes });
        };
        let Some((_, field)) = object.get_field(&name) else {
          return Err(error::Error::Exec {
            expr,
            kind: error::Kind::FieldNotDefined(name.clone()),
          });
        };
        // TODO do we need all the asserts/do we have to evaluate them to json values?
        for (env, assert) in object.asserts() {
          get(&env, ars, assert)?;
        }
        match field {
          Field::Std(field) => match field {
            StdField::ThisFile => todo!("this file"),
            StdField::Fn(f) => Ok(Val::StdFn(f)),
          },
          Field::Expr(env, expr) => get(&env, ars, expr),
        }
      }
      Val::Array(array) => {
        let Val::Prim(Prim::Number(idx)) = get(env, ars, *idx)? else {
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
        #[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
        let idx = idx_floor as u32;
        let Ok(idx) = usize::try_from(idx) else {
          return Err(error::Error::Exec { expr, kind: error::Kind::ArrayIdxOutOfRange });
        };
        match array.get(idx) {
          Some((env, elem)) => get(env, ars, elem),
          None => Err(error::Error::Exec { expr, kind: error::Kind::ArrayIdxOutOfRange }),
        }
      }
      _ => Err(error::Error::Exec { expr, kind: error::Kind::IncompatibleTypes }),
    },
    ExprData::Call { func, positional, named } => match get(env, ars, *func)? {
      Val::Function { env: func_env, mut params, body } => {
        if let Some(tma) = arg::TooMany::new(params.len(), positional.len(), named.len()) {
          return Err(error::Error::Exec { expr, kind: arg::ErrorKind::TooMany(tma).into() });
        }
        let mut provided = FxHashSet::<Id>::default();
        for ((id, param), &arg) in params.iter_mut().zip(positional) {
          *param = Some(arg);
          assert!(provided.insert(*id), "duplicate function param should be forbidden by check");
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
          // arg and short circuits with true. note `==` with comparing the names and `=` with
          // setting the actual exprs. note the usage of `bool::then` with `find_map` and `is_none`.
          let arg_not_requested = params
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
        let mut env = func_env.clone();
        for (id, rhs) in params {
          match rhs {
            // from my (not super close) reading of the spec, it seems like for function parameters
            // without default values, the default value should be set to `error "Parameter not
            // bound"`, which will _lazily_ emit the error if the parameter is accessed. but the
            // behavior of the impl on the website is to _eagerly_ error if a param is not defined.
            // so we do that here.
            None => {
              return Err(error::Error::Exec { expr, kind: arg::ErrorKind::NotDefined(id).into() })
            }
            Some(rhs) => env.insert(id, func_env.clone(), rhs),
          }
        }
        get(&env, ars, body)
      }
      Val::StdFn(std_fn) => get_std_fn(env, ars, positional, named, expr, std_fn),
      _ => Err(error::Error::Exec { expr, kind: error::Kind::IncompatibleTypes }),
    },
    ExprData::Id(id) => match env.get(*id) {
      Get::Self_ => Ok(Val::Object(env.this().clone())),
      Get::Super => Ok(Val::Object(env.this().parent())),
      Get::Std => Ok(Val::Object(Object::std_lib())),
      Get::Expr(env, expr) => get(env, ars, expr),
    },
    ExprData::Local { binds, body } => {
      let env = add_binds(env, binds);
      get(&env, ars, *body)
    }
    ExprData::If { cond, yes, no } => {
      let Val::Prim(Prim::Bool(b)) = get(env, ars, *cond)? else {
        return Err(error::Error::Exec { expr, kind: error::Kind::IncompatibleTypes });
      };
      let &expr = if b { yes } else { no };
      get(env, ars, expr)
    }
    ExprData::BinaryOp { lhs, op, rhs } => match op {
      // add
      BinaryOp::Add => match (get(env, ars, *lhs)?, get(env, ars, *rhs)?) {
        (Val::Prim(Prim::String(lhs)), rhs) => {
          let rhs = str_conv(ars, rhs)?;
          Ok(Val::Prim(Prim::String(str_concat(&ars.str, &lhs, &rhs))))
        }
        (lhs, Val::Prim(Prim::String(rhs))) => {
          let lhs = str_conv(ars, lhs)?;
          Ok(Val::Prim(Prim::String(str_concat(&ars.str, &lhs, &rhs))))
        }
        (Val::Prim(Prim::Number(lhs)), Val::Prim(Prim::Number(rhs))) => {
          let n = match Number::try_from(lhs.value() + rhs.value()) {
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
      BinaryOp::Mul => float_op(expr, env, ars, *lhs, *rhs, std::ops::Mul::mul),
      BinaryOp::Div => float_op(expr, env, ars, *lhs, *rhs, std::ops::Div::div),
      BinaryOp::Sub => float_op(expr, env, ars, *lhs, *rhs, std::ops::Sub::sub),
      // bitwise
      BinaryOp::Shl => int_op(expr, env, ars, *lhs, *rhs, std::ops::Shl::shl),
      BinaryOp::Shr => int_op(expr, env, ars, *lhs, *rhs, std::ops::Shr::shr),
      BinaryOp::BitAnd => int_op(expr, env, ars, *lhs, *rhs, std::ops::BitAnd::bitand),
      BinaryOp::BitXor => int_op(expr, env, ars, *lhs, *rhs, std::ops::BitXor::bitxor),
      BinaryOp::BitOr => int_op(expr, env, ars, *lhs, *rhs, std::ops::BitOr::bitor),
      // comparison
      BinaryOp::Lt => cmp_bool_op(expr, env, ars, *lhs, *rhs, Ordering::is_lt),
      BinaryOp::LtEq => cmp_bool_op(expr, env, ars, *lhs, *rhs, Ordering::is_le),
      BinaryOp::Gt => cmp_bool_op(expr, env, ars, *lhs, *rhs, Ordering::is_gt),
      BinaryOp::GtEq => cmp_bool_op(expr, env, ars, *lhs, *rhs, Ordering::is_ge),
    },
    ExprData::UnaryOp { op, inner } => {
      let inner = get(env, ars, *inner)?;
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
            #[allow(clippy::cast_precision_loss)]
            let n = n.clamp(i64::MIN as f64, i64::MAX as f64);
            #[allow(clippy::cast_possible_truncation)]
            let n = n as i64;
            let n = !n;
            #[allow(clippy::cast_precision_loss)]
            let n = n as f64;
            let n = Number::try_from(n).expect("bitwise not failed");
            Ok(Val::Prim(Prim::Number(n)))
          } else {
            Err(error::Error::Exec { expr, kind: error::Kind::IncompatibleTypes })
          }
        }
      }
    }
    ExprData::Function { params, body } => {
      Ok(Val::Function { env: env.clone(), params: params.clone(), body: *body })
    }
    ExprData::Error(inner) => {
      let val = get(env, ars, *inner)?;
      let msg = str_conv(ars, val)?;
      Err(error::Error::Exec { expr, kind: error::Kind::User(msg) })
    }
    ExprData::Import { .. } => todo!("Import"),
  }
}

fn number_pair(expr: ExprMust, env: &Env, ars: &Arenas, a: Expr, b: Expr) -> Result<[Number; 2]> {
  match (get(env, ars, a)?, get(env, ars, b)?) {
    (Val::Prim(Prim::Number(a)), Val::Prim(Prim::Number(b))) => Ok([a, b]),
    _ => Err(error::Error::Exec { expr, kind: error::Kind::IncompatibleTypes }),
  }
}

fn float_op<F>(expr: ExprMust, env: &Env, ars: &Arenas, lhs: Expr, rhs: Expr, f: F) -> Result<Val>
where
  F: FnOnce(f64, f64) -> f64,
{
  let [lhs, rhs] = number_pair(expr, env, ars, lhs, rhs)?;
  let n = match Number::try_from(f(lhs.value(), rhs.value())) {
    Ok(n) => n,
    Err(inf) => return Err(error::Error::Exec { expr, kind: error::Kind::Infinite(inf) }),
  };
  Ok(Val::Prim(Prim::Number(n)))
}

fn int_op<F>(expr: ExprMust, env: &Env, ars: &Arenas, lhs: Expr, rhs: Expr, f: F) -> Result<Val>
where
  F: FnOnce(i64, i64) -> i64,
{
  let ns = number_pair(expr, env, ars, lhs, rhs)?;
  #[allow(clippy::cast_possible_truncation)]
  let [lhs, rhs] = ns.map(|x| x.value() as i64);
  #[allow(clippy::cast_precision_loss)]
  let n = f(lhs, rhs) as f64;
  let n = match Number::try_from(n) {
    Ok(n) => n,
    Err(inf) => return Err(error::Error::Exec { expr, kind: error::Kind::Infinite(inf) }),
  };
  Ok(Val::Prim(Prim::Number(n)))
}

fn cmp_op<F>(expr: ExprMust, env: &Env, ars: &Arenas, lhs: Expr, rhs: Expr, f: F) -> Result<Val>
where
  F: FnOnce(Ordering) -> Prim,
{
  let lhs = get(env, ars, lhs)?;
  let rhs = get(env, ars, rhs)?;
  let ord = cmp_val(expr, ars, &lhs, &rhs)?;
  Ok(Val::Prim(f(ord)))
}

fn cmp_bool_op<F>(
  expr: ExprMust,
  env: &Env,
  ars: &Arenas,
  lhs: Expr,
  rhs: Expr,
  f: F,
) -> Result<Val>
where
  F: FnOnce(Ordering) -> bool,
{
  cmp_op(expr, env, ars, lhs, rhs, |x| Prim::Bool(f(x)))
}

fn cmp_val(expr: ExprMust, ars: &Arenas, lhs: &Val, rhs: &Val) -> Result<Ordering> {
  match (lhs, rhs) {
    (Val::Prim(lhs), Val::Prim(rhs)) => match (lhs, rhs) {
      (Prim::String(lhs), Prim::String(rhs)) => Ok(ars.str.get(lhs).cmp(ars.str.get(rhs))),
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
            let lhs = get(le, ars, lhs)?;
            let rhs = get(re, ars, rhs)?;
            match cmp_val(expr, ars, &lhs, &rhs)? {
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

fn add_binds(env: &Env, binds: &[(Id, Expr)]) -> Env {
  let mut ret = env.clone();
  for &(id, expr) in binds {
    ret.insert(id, env.clone(), expr);
  }
  ret
}

fn str_conv(ars: &Arenas, val: Val) -> Result<Str> {
  if let Val::Prim(Prim::String(s)) = val {
    Ok(s)
  } else {
    let json = manifest::get(ars, val)?;
    let string = json.display(&ars.str).to_string();
    Ok(ars.str.str_shared(string.into_boxed_str()))
  }
}

fn str_concat(ar: &StrArena, lhs: &Str, rhs: &Str) -> Str {
  let lhs = ar.get(lhs);
  let rhs = ar.get(rhs);
  let both = format!("{lhs}{rhs}").into_boxed_str();
  ar.str_shared(both)
}

fn get_std_fn(
  env: &Env,
  ars: &Arenas,
  positional: &[Expr],
  named: &[(Id, Expr)],
  expr: ExprMust,
  std_fn: StdFn,
) -> Result<Val> {
  match std_fn {
    StdFn::extVar => {
      let _ = std_fn::args::extVar(positional, named, expr)?;
      todo!("std.extVar")
    }
    StdFn::type_ => {
      let _ = std_fn::args::type_(positional, named, expr)?;
      todo!("std.type")
    }
    StdFn::length => {
      let _ = std_fn::args::length(positional, named, expr)?;
      todo!("std.length")
    }
    StdFn::get => {
      let _ = std_fn::args::get(positional, named, expr)?;
      todo!("std.get")
    }
    StdFn::objectHas => {
      let _ = std_fn::args::objectHas(positional, named, expr)?;
      todo!("std.objectHas")
    }
    StdFn::objectFields => {
      let _ = std_fn::args::objectFields(positional, named, expr)?;
      todo!("std.objectFields")
    }
    StdFn::objectValues => {
      let _ = std_fn::args::objectValues(positional, named, expr)?;
      todo!("std.objectValues")
    }
    StdFn::objectKeysValues => {
      let _ = std_fn::args::objectKeysValues(positional, named, expr)?;
      todo!("std.objectKeysValues")
    }
    StdFn::objectHasAll => {
      let _ = std_fn::args::objectHasAll(positional, named, expr)?;
      todo!("std.objectHasAll")
    }
    StdFn::objectFieldsAll => {
      let _ = std_fn::args::objectFieldsAll(positional, named, expr)?;
      todo!("std.objectFieldsAll")
    }
    StdFn::objectValuesAll => {
      let _ = std_fn::args::objectValuesAll(positional, named, expr)?;
      todo!("std.objectValuesAll")
    }
    StdFn::objectKeysValuesAll => {
      let _ = std_fn::args::objectKeysValuesAll(positional, named, expr)?;
      todo!("std.objectKeysValuesAll")
    }
    StdFn::prune => {
      let _ = std_fn::args::prune(positional, named, expr)?;
      todo!("std.prune")
    }
    StdFn::mapWithKey => {
      let _ = std_fn::args::mapWithKey(positional, named, expr)?;
      todo!("std.mapWithKey")
    }
    StdFn::abs => {
      let _ = std_fn::args::abs(positional, named, expr)?;
      todo!("std.abs")
    }
    StdFn::sign => {
      let _ = std_fn::args::sign(positional, named, expr)?;
      todo!("std.sign")
    }
    StdFn::max => {
      let _ = std_fn::args::max(positional, named, expr)?;
      todo!("std.max")
    }
    StdFn::min => {
      let _ = std_fn::args::min(positional, named, expr)?;
      todo!("std.min")
    }
    StdFn::pow => {
      let _ = std_fn::args::pow(positional, named, expr)?;
      todo!("std.pow")
    }
    StdFn::exp => {
      let _ = std_fn::args::exp(positional, named, expr)?;
      todo!("std.exp")
    }
    StdFn::log => {
      let _ = std_fn::args::log(positional, named, expr)?;
      todo!("std.log")
    }
    StdFn::exponent => {
      let _ = std_fn::args::exponent(positional, named, expr)?;
      todo!("std.exponent")
    }
    StdFn::mantissa => {
      let _ = std_fn::args::mantissa(positional, named, expr)?;
      todo!("std.mantissa")
    }
    StdFn::floor => {
      let _ = std_fn::args::floor(positional, named, expr)?;
      todo!("std.floor")
    }
    StdFn::ceil => {
      let _ = std_fn::args::ceil(positional, named, expr)?;
      todo!("std.ceil")
    }
    StdFn::sqrt => {
      let _ = std_fn::args::sqrt(positional, named, expr)?;
      todo!("std.sqrt")
    }
    StdFn::sin => {
      let _ = std_fn::args::sin(positional, named, expr)?;
      todo!("std.sin")
    }
    StdFn::cos => {
      let _ = std_fn::args::cos(positional, named, expr)?;
      todo!("std.cos")
    }
    StdFn::tan => {
      let _ = std_fn::args::tan(positional, named, expr)?;
      todo!("std.tan")
    }
    StdFn::asin => {
      let _ = std_fn::args::asin(positional, named, expr)?;
      todo!("std.asin")
    }
    StdFn::acos => {
      let _ = std_fn::args::acos(positional, named, expr)?;
      todo!("std.acos")
    }
    StdFn::atan => {
      let _ = std_fn::args::atan(positional, named, expr)?;
      todo!("std.atan")
    }
    StdFn::round => {
      let _ = std_fn::args::round(positional, named, expr)?;
      todo!("std.round")
    }
    StdFn::mod_ => {
      let _ = std_fn::args::mod_(positional, named, expr)?;
      todo!("std.mod")
    }
    StdFn::clamp => {
      let _ = std_fn::args::clamp(positional, named, expr)?;
      todo!("std.clamp")
    }
    StdFn::assertEqual => {
      let _ = std_fn::args::assertEqual(positional, named, expr)?;
      todo!("std.assertEqual")
    }
    StdFn::toString => {
      let _ = std_fn::args::toString(positional, named, expr)?;
      todo!("std.toString")
    }
    StdFn::codepoint => {
      let _ = std_fn::args::codepoint(positional, named, expr)?;
      todo!("std.codepoint")
    }
    StdFn::char => {
      let _ = std_fn::args::char(positional, named, expr)?;
      todo!("std.char")
    }
    StdFn::substr => {
      let _ = std_fn::args::substr(positional, named, expr)?;
      todo!("std.substr")
    }
    StdFn::findSubstr => {
      let _ = std_fn::args::findSubstr(positional, named, expr)?;
      todo!("std.findSubstr")
    }
    StdFn::startsWith => {
      let _ = std_fn::args::startsWith(positional, named, expr)?;
      todo!("std.startsWith")
    }
    StdFn::endsWith => {
      let _ = std_fn::args::endsWith(positional, named, expr)?;
      todo!("std.endsWith")
    }
    StdFn::stripChars => {
      let _ = std_fn::args::stripChars(positional, named, expr)?;
      todo!("std.stripChars")
    }
    StdFn::lstripChars => {
      let _ = std_fn::args::lstripChars(positional, named, expr)?;
      todo!("std.lstripChars")
    }
    StdFn::rstripChars => {
      let _ = std_fn::args::rstripChars(positional, named, expr)?;
      todo!("std.rstripChars")
    }
    StdFn::split => {
      let _ = std_fn::args::split(positional, named, expr)?;
      todo!("std.split")
    }
    StdFn::splitLimit => {
      let _ = std_fn::args::splitLimit(positional, named, expr)?;
      todo!("std.splitLimit")
    }
    StdFn::splitLimitR => {
      let _ = std_fn::args::splitLimitR(positional, named, expr)?;
      todo!("std.splitLimitR")
    }
    StdFn::strReplace => {
      let _ = std_fn::args::strReplace(positional, named, expr)?;
      todo!("std.strReplace")
    }
    StdFn::isEmpty => {
      let _ = std_fn::args::isEmpty(positional, named, expr)?;
      todo!("std.isEmpty")
    }
    StdFn::asciiUpper => {
      let _ = std_fn::args::asciiUpper(positional, named, expr)?;
      todo!("std.asciiUpper")
    }
    StdFn::asciiLower => {
      let _ = std_fn::args::asciiLower(positional, named, expr)?;
      todo!("std.asciiLower")
    }
    StdFn::stringChars => {
      let _ = std_fn::args::stringChars(positional, named, expr)?;
      todo!("std.stringChars")
    }
    StdFn::format => {
      let _ = std_fn::args::format(positional, named, expr)?;
      todo!("std.format")
    }
    StdFn::escapeStringBash => {
      let _ = std_fn::args::escapeStringBash(positional, named, expr)?;
      todo!("std.escapeStringBash")
    }
    StdFn::escapeStringDollars => {
      let _ = std_fn::args::escapeStringDollars(positional, named, expr)?;
      todo!("std.escapeStringDollars")
    }
    StdFn::escapeStringJson => {
      let _ = std_fn::args::escapeStringJson(positional, named, expr)?;
      todo!("std.escapeStringJson")
    }
    StdFn::escapeStringPython => {
      let _ = std_fn::args::escapeStringPython(positional, named, expr)?;
      todo!("std.escapeStringPython")
    }
    StdFn::escapeStringXml => {
      let _ = std_fn::args::escapeStringXml(positional, named, expr)?;
      todo!("std.escapeStringXml")
    }
    StdFn::parseInt => {
      let _ = std_fn::args::parseInt(positional, named, expr)?;
      todo!("std.parseInt")
    }
    StdFn::parseOctal => {
      let _ = std_fn::args::parseOctal(positional, named, expr)?;
      todo!("std.parseOctal")
    }
    StdFn::parseHex => {
      let _ = std_fn::args::parseHex(positional, named, expr)?;
      todo!("std.parseHex")
    }
    StdFn::parseJson => {
      let _ = std_fn::args::parseJson(positional, named, expr)?;
      todo!("std.parseJson")
    }
    StdFn::parseYaml => {
      let _ = std_fn::args::parseYaml(positional, named, expr)?;
      todo!("std.parseYaml")
    }
    StdFn::encodeUTF8 => {
      let _ = std_fn::args::encodeUTF8(positional, named, expr)?;
      todo!("std.encodeUTF8")
    }
    StdFn::decodeUTF8 => {
      let _ = std_fn::args::decodeUTF8(positional, named, expr)?;
      todo!("std.decodeUTF8")
    }
    StdFn::manifestIni => {
      let _ = std_fn::args::manifestIni(positional, named, expr)?;
      todo!("std.manifestIni")
    }
    StdFn::manifestPython => {
      let _ = std_fn::args::manifestPython(positional, named, expr)?;
      todo!("std.manifestPython")
    }
    StdFn::manifestPythonVars => {
      let _ = std_fn::args::manifestPythonVars(positional, named, expr)?;
      todo!("std.manifestPythonVars")
    }
    StdFn::manifestJsonEx => {
      let _ = std_fn::args::manifestJsonEx(positional, named, expr)?;
      todo!("std.manifestJsonEx")
    }
    StdFn::manifestJsonMinified => {
      let _ = std_fn::args::manifestJsonMinified(positional, named, expr)?;
      todo!("std.manifestJsonMinified")
    }
    StdFn::manifestYamlDoc => {
      let _ = std_fn::args::manifestYamlDoc(positional, named, expr)?;
      todo!("std.manifestYamlDoc")
    }
    StdFn::manifestYamlStream => {
      let _ = std_fn::args::manifestYamlStream(positional, named, expr)?;
      todo!("std.manifestYamlStream")
    }
    StdFn::manifestXmlJsonml => {
      let _ = std_fn::args::manifestXmlJsonml(positional, named, expr)?;
      todo!("std.manifestXmlJsonml")
    }
    StdFn::manifestTomlEx => {
      let _ = std_fn::args::manifestTomlEx(positional, named, expr)?;
      todo!("std.manifestTomlEx")
    }
    StdFn::makeArray => {
      let _ = std_fn::args::makeArray(positional, named, expr)?;
      todo!("std.makeArray")
    }
    StdFn::member => {
      let _ = std_fn::args::member(positional, named, expr)?;
      todo!("std.member")
    }
    StdFn::count => {
      let _ = std_fn::args::count(positional, named, expr)?;
      todo!("std.count")
    }
    StdFn::find => {
      let _ = std_fn::args::find(positional, named, expr)?;
      todo!("std.find")
    }
    StdFn::map => {
      let _ = std_fn::args::map(positional, named, expr)?;
      todo!("std.map")
    }
    StdFn::mapWithIndex => {
      let _ = std_fn::args::mapWithIndex(positional, named, expr)?;
      todo!("std.mapWithIndex")
    }
    StdFn::filterMap => {
      let _ = std_fn::args::filterMap(positional, named, expr)?;
      todo!("std.filterMap")
    }
    StdFn::flatMap => {
      let _ = std_fn::args::flatMap(positional, named, expr)?;
      todo!("std.flatMap")
    }
    StdFn::filter => {
      let _ = std_fn::args::filter(positional, named, expr)?;
      todo!("std.filter")
    }
    StdFn::foldl => {
      let _ = std_fn::args::foldl(positional, named, expr)?;
      todo!("std.foldl")
    }
    StdFn::foldr => {
      let _ = std_fn::args::foldr(positional, named, expr)?;
      todo!("std.foldr")
    }
    StdFn::range => {
      let _ = std_fn::args::range(positional, named, expr)?;
      todo!("std.range")
    }
    StdFn::repeat => {
      let _ = std_fn::args::repeat(positional, named, expr)?;
      todo!("std.repeat")
    }
    StdFn::slice => {
      let _ = std_fn::args::slice(positional, named, expr)?;
      todo!("std.slice")
    }
    StdFn::join => {
      let _ = std_fn::args::join(positional, named, expr)?;
      todo!("std.join")
    }
    StdFn::lines => {
      let _ = std_fn::args::lines(positional, named, expr)?;
      todo!("std.lines")
    }
    StdFn::flattenArrays => {
      let _ = std_fn::args::flattenArrays(positional, named, expr)?;
      todo!("std.flattenArrays")
    }
    StdFn::reverse => {
      let _ = std_fn::args::reverse(positional, named, expr)?;
      todo!("std.reverse")
    }
    StdFn::sort => {
      let _ = std_fn::args::sort(positional, named, expr)?;
      todo!("std.sort")
    }
    StdFn::uniq => {
      let _ = std_fn::args::uniq(positional, named, expr)?;
      todo!("std.uniq")
    }
    StdFn::all => {
      let _ = std_fn::args::all(positional, named, expr)?;
      todo!("std.all")
    }
    StdFn::any => {
      let _ = std_fn::args::any(positional, named, expr)?;
      todo!("std.any")
    }
    StdFn::sum => {
      let _ = std_fn::args::sum(positional, named, expr)?;
      todo!("std.sum")
    }
    StdFn::set => {
      let _ = std_fn::args::set(positional, named, expr)?;
      todo!("std.set")
    }
    StdFn::setInter => {
      let _ = std_fn::args::setInter(positional, named, expr)?;
      todo!("std.setInter")
    }
    StdFn::setUnion => {
      let _ = std_fn::args::setUnion(positional, named, expr)?;
      todo!("std.setUnion")
    }
    StdFn::setDiff => {
      let _ = std_fn::args::setDiff(positional, named, expr)?;
      todo!("std.setDiff")
    }
    StdFn::setMember => {
      let _ = std_fn::args::setMember(positional, named, expr)?;
      todo!("std.setMember")
    }
    StdFn::base64 => {
      let _ = std_fn::args::base64(positional, named, expr)?;
      todo!("std.base64")
    }
    StdFn::base64DecodeBytes => {
      let _ = std_fn::args::base64DecodeBytes(positional, named, expr)?;
      todo!("std.base64DecodeBytes")
    }
    StdFn::base64Decode => {
      let _ = std_fn::args::base64Decode(positional, named, expr)?;
      todo!("std.base64Decode")
    }
    StdFn::md5 => {
      let _ = std_fn::args::md5(positional, named, expr)?;
      todo!("std.md5")
    }
    StdFn::xor => {
      let _ = std_fn::args::xor(positional, named, expr)?;
      todo!("std.xor")
    }
    StdFn::xnor => {
      let _ = std_fn::args::xnor(positional, named, expr)?;
      todo!("std.xnor")
    }
    StdFn::mergePatch => {
      let _ = std_fn::args::mergePatch(positional, named, expr)?;
      todo!("std.mergePatch")
    }
    StdFn::trace => {
      let _ = std_fn::args::trace(positional, named, expr)?;
      todo!("std.trace")
    }
    StdFn::cmp => {
      let arguments = std_fn::args::cmp(positional, named, expr)?;
      cmp_op(expr, env, ars, arguments.a, arguments.b, |ord| {
        let num = match ord {
          Ordering::Less => Number::negative_one(),
          Ordering::Equal => Number::positive_zero(),
          Ordering::Greater => Number::positive_one(),
        };
        Prim::Number(num)
      })
    }
    StdFn::equals => {
      let arguments = std_fn::args::equals(positional, named, expr)?;
      cmp_bool_op(expr, env, ars, arguments.a, arguments.b, Ordering::is_eq)
    }
    StdFn::objectHasEx => {
      let _ = std_fn::args::objectHasEx(positional, named, expr)?;
      todo!("std.objectHasEx")
    }
  }
}
