//! Executing Jsonnet expression to produce Jsonnet values.

use crate::error::{self, Result};
use crate::manifest;
use crate::val::jsonnet::{Array, Env, Field, Get, Object, StdField, Val};
use jsonnet_expr::{
  Arenas, BinaryOp, Expr, ExprData, ExprMust, Id, Number, Prim, StdFn, Str, StrArena, Visibility,
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
        if let Some(tma) = error::TooManyArgs::new(params.len(), positional.len(), named.len()) {
          return Err(error::Error::Exec { expr, kind: error::Kind::TooManyArgs(tma) });
        }
        let mut provided = FxHashSet::<Id>::default();
        for ((id, param), &arg) in params.iter_mut().zip(positional) {
          *param = Some(arg);
          assert!(provided.insert(*id), "duplicate function param should be forbidden by check");
        }
        for &(arg_name, arg) in named {
          if !provided.insert(arg_name) {
            return Err(error::Error::Exec {
              expr,
              kind: error::Kind::DuplicateArgument(arg_name),
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
            return Err(error::Error::Exec { expr, kind: error::Kind::ArgNotRequested(arg_name) });
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
              return Err(error::Error::Exec { expr, kind: error::Kind::ParamNotDefined(id) })
            }
            Some(rhs) => env.insert(id, func_env.clone(), rhs),
          }
        }
        get(&env, ars, body)
      }
      Val::StdFn(std_fn) => match std_fn {
        StdFn::extVar => todo!("std.extVar"),
        StdFn::type_ => todo!("std.type"),
        StdFn::length => todo!("std.length"),
        StdFn::get => todo!("std.get"),
        StdFn::objectHas => todo!("std.objectHas"),
        StdFn::objectFields => todo!("std.objectFields"),
        StdFn::objectValues => todo!("std.objectValues"),
        StdFn::objectKeysValues => todo!("std.objectKeysValues"),
        StdFn::objectHasAll => todo!("std.objectHasAll"),
        StdFn::objectFieldsAll => todo!("std.objectFieldsAll"),
        StdFn::objectValuesAll => todo!("std.objectValuesAll"),
        StdFn::objectKeysValuesAll => todo!("std.objectKeysValuesAll"),
        StdFn::prune => todo!("std.prune"),
        StdFn::mapWithKey => todo!("std.mapWithKey"),
        StdFn::abs => todo!("std.abs"),
        StdFn::sign => todo!("std.sign"),
        StdFn::max => todo!("std.max"),
        StdFn::min => todo!("std.min"),
        StdFn::pow => todo!("std.pow"),
        StdFn::exp => todo!("std.exp"),
        StdFn::log => todo!("std.log"),
        StdFn::exponent => todo!("std.exponent"),
        StdFn::mantissa => todo!("std.mantissa"),
        StdFn::floor => todo!("std.floor"),
        StdFn::ceil => todo!("std.ceil"),
        StdFn::sqrt => todo!("std.sqrt"),
        StdFn::sin => todo!("std.sin"),
        StdFn::cos => todo!("std.cos"),
        StdFn::tan => todo!("std.tan"),
        StdFn::asin => todo!("std.asin"),
        StdFn::acos => todo!("std.acos"),
        StdFn::atan => todo!("std.atan"),
        StdFn::round => todo!("std.round"),
        StdFn::mod_ => todo!("std.mod"),
        StdFn::clamp => todo!("std.clamp"),
        StdFn::assertEqual => todo!("std.assertEqual"),
        StdFn::toString => todo!("std.toString"),
        StdFn::codepoint => todo!("std.codepoint"),
        StdFn::char => todo!("std.char"),
        StdFn::substr => todo!("std.substr"),
        StdFn::findSubstr => todo!("std.findSubstr"),
        StdFn::startsWith => todo!("std.startsWith"),
        StdFn::endsWith => todo!("std.endsWith"),
        StdFn::stripChars => todo!("std.stripChars"),
        StdFn::lstripChars => todo!("std.lstripChars"),
        StdFn::rstripChars => todo!("std.rstripChars"),
        StdFn::split => todo!("std.split"),
        StdFn::splitLimit => todo!("std.splitLimit"),
        StdFn::splitLimitR => todo!("std.splitLimitR"),
        StdFn::strReplace => todo!("std.strReplace"),
        StdFn::isEmpty => todo!("std.isEmpty"),
        StdFn::asciiUpper => todo!("std.asciiUpper"),
        StdFn::asciiLower => todo!("std.asciiLower"),
        StdFn::stringChars => todo!("std.stringChars"),
        StdFn::format => todo!("std.format"),
        StdFn::escapeStringBash => todo!("std.escapeStringBash"),
        StdFn::escapeStringDollars => todo!("std.escapeStringDollars"),
        StdFn::escapeStringJson => todo!("std.escapeStringJson"),
        StdFn::escapeStringPython => todo!("std.escapeStringPython"),
        StdFn::escapeStringXml => todo!("std.escapeStringXml"),
        StdFn::parseInt => todo!("std.parseInt"),
        StdFn::parseOctal => todo!("std.parseOctal"),
        StdFn::parseHex => todo!("std.parseHex"),
        StdFn::parseJson => todo!("std.parseJson"),
        StdFn::parseYaml => todo!("std.parseYaml"),
        StdFn::encodeUTF8 => todo!("std.encodeUTF8"),
        StdFn::decodeUTF8 => todo!("std.decodeUTF8"),
        StdFn::manifestIni => todo!("std.manifestIni"),
        StdFn::manifestPython => todo!("std.manifestPython"),
        StdFn::manifestPythonVars => todo!("std.manifestPythonVars"),
        StdFn::manifestJsonEx => todo!("std.manifestJsonEx"),
        StdFn::manifestJsonMinified => todo!("std.manifestJsonMinified"),
        StdFn::manifestYamlDoc => todo!("std.manifestYamlDoc"),
        StdFn::manifestYamlStream => todo!("std.manifestYamlStream"),
        StdFn::manifestXmlJsonml => todo!("std.manifestXmlJsonml"),
        StdFn::manifestTomlEx => todo!("std.manifestTomlEx"),
        StdFn::makeArray => todo!("std.makeArray"),
        StdFn::member => todo!("std.member"),
        StdFn::count => todo!("std.count"),
        StdFn::find => todo!("std.find"),
        StdFn::map => todo!("std.map"),
        StdFn::mapWithIndex => todo!("std.mapWithIndex"),
        StdFn::filterMap => todo!("std.filterMap"),
        StdFn::flatMap => todo!("std.flatMap"),
        StdFn::filter => todo!("std.filter"),
        StdFn::foldl => todo!("std.foldl"),
        StdFn::foldr => todo!("std.foldr"),
        StdFn::range => todo!("std.range"),
        StdFn::repeat => todo!("std.repeat"),
        StdFn::slice => todo!("std.slice"),
        StdFn::join => todo!("std.join"),
        StdFn::lines => todo!("std.lines"),
        StdFn::flattenArrays => todo!("std.flattenArrays"),
        StdFn::reverse => todo!("std.reverse"),
        StdFn::sort => todo!("std.sort"),
        StdFn::uniq => todo!("std.uniq"),
        StdFn::all => todo!("std.all"),
        StdFn::any => todo!("std.any"),
        StdFn::sum => todo!("std.sum"),
        StdFn::set => todo!("std.set"),
        StdFn::setInter => todo!("std.setInter"),
        StdFn::setUnion => todo!("std.setUnion"),
        StdFn::setDiff => todo!("std.setDiff"),
        StdFn::setMember => todo!("std.setMember"),
        StdFn::base64 => todo!("std.base64"),
        StdFn::base64DecodeBytes => todo!("std.base64DecodeBytes"),
        StdFn::base64Decode => todo!("std.base64Decode"),
        StdFn::md5 => todo!("std.md5"),
        StdFn::xor => todo!("std.xor"),
        StdFn::xnor => todo!("std.xnor"),
        StdFn::mergePatch => todo!("std.mergePatch"),
        StdFn::trace => todo!("std.trace"),
        StdFn::cmp => {
          let [a, b] = get_a_b(positional, named, expr)?;
          // named.get(index)
          cmp_op(expr, env, ars, a, b, |ord| {
            let num = match ord {
              Ordering::Less => Number::negative_one(),
              Ordering::Equal => Number::positive_zero(),
              Ordering::Greater => Number::positive_one(),
            };
            Prim::Number(num)
          })
        }
        StdFn::equals => {
          let [a, b] = get_a_b(positional, named, expr)?;
          cmp_bool_op(expr, env, ars, a, b, Ordering::is_eq)
        }
        StdFn::objectHasEx => todo!("std.objectHasEx"),
      },
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

fn get_a_b(positional: &[Expr], named: &[(Id, Expr)], expr: ExprMust) -> Result<[Expr; 2]> {
  if let Some(tma) = error::TooManyArgs::new(2, positional.len(), named.len()) {
    return Err(error::Error::Exec { expr, kind: error::Kind::TooManyArgs(tma) });
  }
  let mut positional = positional.iter().copied();
  let mut a = positional.next().flatten();
  let mut b = positional.next().flatten();
  for &(arg_name, arg) in named {
    if arg_name == Id::a {
      match a {
        None => a = arg,
        Some(_) => {
          return Err(error::Error::Exec { expr, kind: error::Kind::DuplicateArgument(arg_name) })
        }
      }
    } else if arg_name == Id::b {
      match b {
        None => b = arg,
        Some(_) => {
          return Err(error::Error::Exec { expr, kind: error::Kind::DuplicateArgument(arg_name) })
        }
      }
    } else {
      return Err(error::Error::Exec { expr, kind: error::Kind::ArgNotRequested(arg_name) });
    }
  }
  Ok([a, b])
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
