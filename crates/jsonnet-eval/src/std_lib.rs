//! The standard library for Jsonnet, implemented in Rust.

use crate::error::{self, Error, Result};
use crate::util;
use crate::{Cx, exec, generated::fns, mk_todo};
use always::always;
use finite_float::Float;
use jsonnet_expr::{Expr, ExprArena, ExprData, ExprMust, Id, Prim, StdFn, Str, Vis};
use jsonnet_val::jsonnet::{Array, Env, ExprField, ExprFields, Fn, Subst, Val, ValOrExpr};
use rustc_hash::FxHashSet;

pub(crate) fn get_call(
  cx: &mut Cx<'_>,
  env: &Env,
  expr: ExprMust,
  func: StdFn,
  pos: &[Expr],
  named: &[(Id, Expr)],
) -> Result<Val> {
  match func {
    StdFn::type_ => {
      let arg = fns::type_::new(pos, named, expr)?.x(cx, env)?;
      let ret = match arg {
        Val::Prim(prim) => match prim {
          Prim::Null => Str::null,
          Prim::Bool(_) => Str::boolean,
          Prim::String(_) => Str::string,
          Prim::Number(_) => Str::number,
        },
        Val::Object(_) => Str::object,
        Val::Array(_) => Str::array,
        Val::Fn(_) => Str::function,
      };
      Ok(ret.into())
    }

    StdFn::isArray => {
      let arg = fns::isArray::new(pos, named, expr)?.v(cx, env)?;
      Ok(matches!(arg, Val::Array(_)).into())
    }

    StdFn::isBoolean => {
      let arg = fns::isBoolean::new(pos, named, expr)?.v(cx, env)?;
      Ok(matches!(arg, Val::Prim(Prim::Bool(_))).into())
    }

    StdFn::isFunction => {
      let arg = fns::isFunction::new(pos, named, expr)?.v(cx, env)?;
      Ok(matches!(arg, Val::Fn(_)).into())
    }

    StdFn::isNumber => {
      let arg = fns::isNumber::new(pos, named, expr)?.v(cx, env)?;
      Ok(matches!(arg, Val::Prim(Prim::Number(_))).into())
    }

    StdFn::isObject => {
      let arg = fns::isObject::new(pos, named, expr)?.v(cx, env)?;
      Ok(matches!(arg, Val::Object(_)).into())
    }

    StdFn::isString => {
      let arg = fns::isString::new(pos, named, expr)?.v(cx, env)?;
      Ok(matches!(arg, Val::Prim(Prim::String(_))).into())
    }

    StdFn::length => {
      let ret = match fns::length::new(pos, named, expr)?.x(cx, env)? {
        Val::Prim(prim) => match prim {
          Prim::Null | Prim::Bool(_) | Prim::Number(_) => {
            return Err(Error::Exec { expr, kind: error::Kind::IncompatibleTypes });
          }
          // we want "number of codepoints", NOT byte length.
          Prim::String(s) => cx.str_ar.get(s).chars().count(),
        },
        Val::Object(obj) => obj.visible_fields().len(),
        Val::Array(arr) => arr.len(),
        Val::Fn(Fn::Regular(func)) => func.params.iter().filter(|(_, d)| d.is_none()).count(),
        Val::Fn(Fn::Std(func)) => func.required_params_count(),
      };
      Ok(Float::from(always::convert::usize_to_u32(ret)).into())
    }

    StdFn::join => {
      let args = fns::join::new(pos, named, expr)?;
      let arr = args.arr(cx, env)?;
      match args.sep(cx, env)? {
        Val::Prim(Prim::String(sep)) => {
          let mut ret = String::new();
          let sep = cx.str_ar.get(sep).to_owned();
          let mut first = true;
          for (elem_env, elem_expr) in arr.elems() {
            if !first {
              ret.push_str(sep.as_str());
            }
            first = false;
            let val = exec::get(cx, elem_env, elem_expr)?;
            let Val::Prim(Prim::String(elem)) = val else {
              return Err(error::Error::Exec {
                expr: elem_expr.unwrap_or(expr),
                kind: error::Kind::IncompatibleTypes,
              });
            };
            ret.push_str(cx.str_ar.get(elem));
          }
          Ok(util::mk_str(cx.str_ar, ret))
        }
        Val::Array(sep) => {
          let mut ret = Array::default();
          let mut first = true;
          for (elem_env, elem_expr) in arr.elems() {
            if !first {
              ret.append(&mut sep.clone());
            }
            first = false;
            let val = exec::get(cx, elem_env, elem_expr)?;
            let Val::Array(mut elem) = val else {
              return Err(error::Error::Exec {
                expr: elem_expr.unwrap_or(expr),
                kind: error::Kind::IncompatibleTypes,
              });
            };
            ret.append(&mut elem);
          }
          Ok(Val::Array(ret))
        }
        Val::Prim(_) | Val::Object(_) | Val::Fn(_) => {
          Err(error::Error::Exec { expr, kind: error::Kind::IncompatibleTypes })
        }
      }
    }

    StdFn::sign => {
      let n = fns::sign::new(pos, named, expr)?.n(cx, env)?;
      let ret = if n == 0.0 {
        Float::POSITIVE_ZERO
      } else if n.is_sign_positive() {
        Float::POSITIVE_ONE
      } else {
        Float::NEGATIVE_ONE
      };
      Ok(ret.into())
    }

    StdFn::max => {
      let args = fns::max::new(pos, named, expr)?;
      let a = args.a(cx, env)?;
      let b = args.b(cx, env)?;
      util::mk_num(a.max(b), expr)
    }

    StdFn::min => {
      let args = fns::min::new(pos, named, expr)?;
      let a = args.a(cx, env)?;
      let b = args.b(cx, env)?;
      util::mk_num(a.min(b), expr)
    }

    StdFn::pow => {
      let args = fns::pow::new(pos, named, expr)?;
      let x = args.x(cx, env)?;
      let n = args.n(cx, env)?;
      util::mk_num(x.powf(n), expr)
    }

    StdFn::exp => {
      let x = fns::exp::new(pos, named, expr)?.x(cx, env)?;
      util::mk_num(x.exp(), expr)
    }

    StdFn::log => {
      let x = fns::log::new(pos, named, expr)?.x(cx, env)?;
      util::mk_num(x.ln(), expr)
    }

    StdFn::log2 => {
      let x = fns::log2::new(pos, named, expr)?.x(cx, env)?;
      util::mk_num(x.log2(), expr)
    }

    StdFn::log10 => {
      let x = fns::log10::new(pos, named, expr)?.x(cx, env)?;
      util::mk_num(x.log10(), expr)
    }

    StdFn::abs => {
      let n = fns::abs::new(pos, named, expr)?.n(cx, env)?;
      util::mk_num(n.abs(), expr)
    }

    StdFn::floor => {
      let x = fns::floor::new(pos, named, expr)?.x(cx, env)?;
      util::mk_num(x.floor(), expr)
    }

    StdFn::ceil => {
      let x = fns::ceil::new(pos, named, expr)?.x(cx, env)?;
      util::mk_num(x.ceil(), expr)
    }

    StdFn::sqrt => {
      let x = fns::sqrt::new(pos, named, expr)?.x(cx, env)?;
      util::mk_num(x.sqrt(), expr)
    }

    StdFn::sin => {
      let x = fns::sin::new(pos, named, expr)?.x(cx, env)?;
      util::mk_num(x.sin(), expr)
    }

    StdFn::cos => {
      let x = fns::cos::new(pos, named, expr)?.x(cx, env)?;
      util::mk_num(x.cos(), expr)
    }

    StdFn::tan => {
      let x = fns::tan::new(pos, named, expr)?.x(cx, env)?;
      util::mk_num(x.tan(), expr)
    }

    StdFn::asin => {
      let x = fns::asin::new(pos, named, expr)?.x(cx, env)?;
      util::mk_num(x.asin(), expr)
    }

    StdFn::acos => {
      let x = fns::acos::new(pos, named, expr)?.x(cx, env)?;
      util::mk_num(x.acos(), expr)
    }

    StdFn::atan => {
      let x = fns::atan::new(pos, named, expr)?.x(cx, env)?;
      util::mk_num(x.atan(), expr)
    }

    StdFn::atan2 => {
      let args = fns::atan2::new(pos, named, expr)?;
      let y = args.y(cx, env)?;
      let x = args.x(cx, env)?;
      util::mk_num(y.atan2(x), expr)
    }

    StdFn::hypot => {
      let args = fns::hypot::new(pos, named, expr)?;
      let a = args.a(cx, env)?;
      let b = args.b(cx, env)?;
      util::mk_num(a.hypot(b), expr)
    }

    StdFn::deg2rad => {
      let x = fns::deg2rad::new(pos, named, expr)?.x(cx, env)?;
      util::mk_num(x.to_radians(), expr)
    }

    StdFn::rad2deg => {
      let x = fns::rad2deg::new(pos, named, expr)?.x(cx, env)?;
      util::mk_num(x.to_degrees(), expr)
    }

    StdFn::round => {
      let x = fns::round::new(pos, named, expr)?.x(cx, env)?;
      util::mk_num(x.round(), expr)
    }

    StdFn::equals => {
      let args = fns::equals::new(pos, named, expr)?;
      let x = args.x(cx, env)?;
      let y = args.y(cx, env)?;
      Ok(exec::eq_val(expr, cx, &x, &y)?.into())
    }

    StdFn::equalsIgnoreCase => {
      let args = fns::equalsIgnoreCase::new(pos, named, expr)?;
      let a = args.str1(cx, env)?;
      let b = args.str2(cx, env)?;
      let a = cx.str_ar.get(a);
      let b = cx.str_ar.get(b);
      Ok(a.eq_ignore_ascii_case(b).into())
    }

    StdFn::assertEqual => {
      let args = fns::assertEqual::new(pos, named, expr)?;
      let a = args.a(cx, env)?;
      let b = args.b(cx, env)?;
      if exec::eq_val(expr, cx, &a, &b)? {
        Ok(true.into())
      } else {
        Err(error::Error::Exec { expr, kind: error::Kind::User(Str::ASSERTION_FAILED) })
      }
    }

    StdFn::isEven => {
      let x = fns::isEven::new(pos, named, expr)?.x(cx, env)?;
      Ok((x.abs() % 2.0 == 0.0).into())
    }

    StdFn::isOdd => {
      let x = fns::isOdd::new(pos, named, expr)?.x(cx, env)?;
      #[expect(clippy::float_cmp)]
      Ok((x.abs() % 2.0 == 1.0).into())
    }

    StdFn::isInteger => {
      let x = fns::isInteger::new(pos, named, expr)?.x(cx, env)?;
      #[expect(clippy::float_cmp)]
      Ok((x.trunc() == x).into())
    }

    StdFn::isDecimal => {
      let x = fns::isDecimal::new(pos, named, expr)?.x(cx, env)?;
      #[expect(clippy::float_cmp)]
      Ok((x.trunc() != x).into())
    }

    StdFn::clamp => {
      let args = fns::clamp::new(pos, named, expr)?;
      let x = args.x(cx, env)?;
      let min = args.minVal(cx, env)?;
      let max = args.maxVal(cx, env)?;
      util::mk_num(x.clamp(min, max), expr)
    }

    StdFn::isEmpty => {
      let s = fns::isEmpty::new(pos, named, expr)?.str(cx, env)?;
      Ok(cx.str_ar.get(s).is_empty().into())
    }

    StdFn::asciiUpper => {
      let s = fns::asciiUpper::new(pos, named, expr)?.str(cx, env)?;
      let ret = cx.str_ar.get(s).to_ascii_uppercase();
      Ok(util::mk_str(cx.str_ar, ret))
    }

    StdFn::asciiLower => {
      let s = fns::asciiLower::new(pos, named, expr)?.str(cx, env)?;
      let ret = cx.str_ar.get(s).to_ascii_lowercase();
      Ok(util::mk_str(cx.str_ar, ret))
    }

    StdFn::strReplace => {
      let args = fns::strReplace::new(pos, named, expr)?;
      let str = args.str(cx, env)?;
      let from = args.from(cx, env)?;
      let to = args.to(cx, env)?;
      let str = cx.str_ar.get(str);
      let from = cx.str_ar.get(from);
      let to = cx.str_ar.get(to);
      let ret = str.replace(from, to);
      Ok(util::mk_str(cx.str_ar, ret))
    }

    StdFn::substr => {
      let args = fns::substr::new(pos, named, expr)?;
      let str = args.str(cx, env)?;
      let from = always::convert::u32_to_usize(args.from(cx, env)?);
      let len = always::convert::u32_to_usize(args.len(cx, env)?);
      let str = cx.str_ar.get(str);
      if from >= str.len() {
        return Err(error::Error::Exec { expr, kind: error::Kind::IdxOutOfRange(from) });
      }
      let Some(fst) = str.get(from..) else {
        return Err(error::Error::Exec { expr, kind: error::Kind::IdxNotUtf8Boundary(from) });
      };
      let ret = if fst.len() < len {
        fst.to_owned()
      } else {
        match fst.get(..len) {
          Some(x) => x.to_owned(),
          None => {
            return Err(error::Error::Exec { expr, kind: error::Kind::IdxNotUtf8Boundary(len) });
          }
        }
      };
      Ok(util::mk_str(cx.str_ar, ret))
    }

    StdFn::startsWith => {
      let args = fns::startsWith::new(pos, named, expr)?;
      let a = args.a(cx, env)?;
      let b = args.b(cx, env)?;
      let a = cx.str_ar.get(a);
      let b = cx.str_ar.get(b);
      Ok(a.starts_with(b).into())
    }

    StdFn::endsWith => {
      let args = fns::endsWith::new(pos, named, expr)?;
      let a = args.a(cx, env)?;
      let b = args.b(cx, env)?;
      let a = cx.str_ar.get(a);
      let b = cx.str_ar.get(b);
      Ok(a.ends_with(b).into())
    }

    StdFn::stripChars => {
      let args = fns::stripChars::new(pos, named, expr)?;
      let cs = args.chars(cx, env)?;
      let s = args.str(cx, env)?;
      let cs = cx.str_ar.get(cs);
      let s = cx.str_ar.get(s);
      let cs: FxHashSet<_> = cs.chars().collect();
      let ret = s.trim_matches(|c| cs.contains(&c)).to_owned();
      Ok(util::mk_str(cx.str_ar, ret))
    }

    StdFn::lstripChars => {
      let args = fns::lstripChars::new(pos, named, expr)?;
      let cs = args.chars(cx, env)?;
      let s = args.str(cx, env)?;
      let cs = cx.str_ar.get(cs);
      let s = cx.str_ar.get(s);
      let cs: FxHashSet<_> = cs.chars().collect();
      let ret = s.trim_start_matches(|c| cs.contains(&c)).to_owned();
      Ok(util::mk_str(cx.str_ar, ret))
    }

    StdFn::rstripChars => {
      let args = fns::rstripChars::new(pos, named, expr)?;
      let cs = args.chars(cx, env)?;
      let s = args.str(cx, env)?;
      let cs = cx.str_ar.get(cs);
      let s = cx.str_ar.get(s);
      let cs: FxHashSet<_> = cs.chars().collect();
      let ret = s.trim_end_matches(|c| cs.contains(&c)).to_owned();
      Ok(util::mk_str(cx.str_ar, ret))
    }

    StdFn::xor => {
      let args = fns::xor::new(pos, named, expr)?;
      let x = args.x(cx, env)?;
      let y = args.y(cx, env)?;
      Ok((x != y).into())
    }

    StdFn::xnor => {
      let args = fns::xnor::new(pos, named, expr)?;
      let x = args.x(cx, env)?;
      let y = args.y(cx, env)?;
      Ok((x == y).into())
    }

    StdFn::objectHas => {
      let args = fns::objectHas::new(pos, named, expr)?;
      let o = args.o(cx, env)?;
      let f = args.f(cx, env)?;
      Ok(o.get_field(f).is_some_and(|f| f.is_visible()).into())
    }

    StdFn::objectHasAll => {
      let args = fns::objectHasAll::new(pos, named, expr)?;
      let o = args.o(cx, env)?;
      let f = args.f(cx, env)?;
      Ok(o.get_field(f).is_some().into())
    }

    StdFn::objectHasEx => {
      let args = fns::objectHasEx::new(pos, named, expr)?;
      let o = args.obj(cx, env)?;
      let f = args.fname(cx, env)?;
      let hidden = args.hidden(cx, env)?;
      Ok(o.get_field(f).is_some_and(|f| hidden || f.is_visible()).into())
    }

    StdFn::makeArray => {
      let args = fns::makeArray::new(pos, named, expr)?;
      let sz = args.sz(cx, env)?;
      let func = args.func(cx, env)?;
      let mut env = env.clone();
      let func = Some(lift_val(cx, &mut env, Val::Fn(func))?);
      let exprs = path_exprs(cx, env.path())?;
      let elems = (0..sz).map(|idx| {
        let idx = ExprData::Prim(Prim::Number(Float::from(idx)));
        let idx = Some(exprs.ar.alloc(idx));
        let call = ExprData::Call { func, positional: vec![idx], named: Vec::new() };
        Some(exprs.ar.alloc(call))
      });
      Ok(Array::new(env, elems.collect()).into())
    }

    StdFn::get => {
      let args = fns::get::new(pos, named, expr)?;
      let obj = args.o(cx, env)?;
      let field = args.f(cx, env)?;
      let inc_hidden = args.inc_hidden(cx, env)?;
      let Some(field) = obj.get_field(field) else { return args.default(cx, env) };
      if field.is_visible() || inc_hidden {
        exec::ck_object_asserts(cx, &obj)?;
        exec::get_field(cx, env.path(), field)
      } else {
        args.default(cx, env)
      }
    }

    StdFn::format => {
      let args = fns::format::new(pos, named, expr)?;
      let val = args.vals(cx, env)?;
      let str = args.str(cx, env)?;
      let str = cx.str_ar.get(str);
      let elems: Vec<_> = match jsonnet_format_parse::get(str) {
        Ok(es) => es,
        Err(e) => {
          return Err(error::Error::Exec { expr, kind: error::Kind::FormatParse(e) });
        }
      };
      let val = crate::manifest::get(cx, val)?;
      match jsonnet_format_render::get(cx.str_ar, &elems, &val) {
        Ok(x) => Ok(x.into()),
        Err(e) => Err(error::Error::Exec { expr, kind: error::Kind::FormatRender(e) }),
      }
    }

    StdFn::objectFields => {
      let args = fns::objectFields::new(pos, named, expr)?;
      let o = args.o(cx, env)?;
      let fields = o.sorted_visible_fields(cx.str_ar);
      let exprs = path_exprs(cx, env.path())?;
      let elems: Vec<_> = fields
        .into_iter()
        .map(|(s, _, _)| Some(exprs.ar.alloc(ExprData::Prim(Prim::String(s)))))
        .collect();
      Ok(Array::new(Env::empty(env.path()), elems).into())
    }

    StdFn::objectFieldsAll => {
      let args = fns::objectFieldsAll::new(pos, named, expr)?;
      let o = args.o(cx, env)?;
      let fields = o.all_sorted_fields(cx.str_ar);
      let exprs = path_exprs(cx, env.path())?;
      let elems: Vec<_> = fields
        .into_iter()
        .map(|(s, _)| Some(exprs.ar.alloc(ExprData::Prim(Prim::String(s)))))
        .collect();
      Ok(Array::new(Env::empty(env.path()), elems).into())
    }

    StdFn::objectValues => {
      let args = fns::objectValues::new(pos, named, expr)?;
      let o = args.o(cx, env)?;
      let fields = o.sorted_visible_fields(cx.str_ar);
      let mut ret = Array::default();
      for (_, env, expr) in fields {
        ret.push(env, expr);
      }
      Ok(ret.into())
    }

    StdFn::objectValuesAll => {
      let args = fns::objectValuesAll::new(pos, named, expr)?;
      let o = args.o(cx, env)?;
      let fields = o.all_sorted_fields(cx.str_ar);
      let exprs = path_exprs(cx, env.path())?;
      let mut ret = Array::default();
      for (_, field) in fields {
        let (env, expr) = field.into_expr(&mut exprs.ar, env.path());
        ret.push(env, expr);
      }
      Ok(ret.into())
    }

    StdFn::objectKeysValues => {
      let args = fns::objectKeysValues::new(pos, named, expr)?;
      let o = args.o(cx, env)?;
      let fields = o.sorted_visible_fields(cx.str_ar);
      let exprs = path_exprs(cx, env.path())?;
      let mut ret = Array::default();
      for (key, env, expr) in fields {
        let elem = key_value_obj(&mut exprs.ar, key, expr);
        ret.push(env, Some(elem));
      }
      Ok(ret.into())
    }

    StdFn::objectKeysValuesAll => {
      let args = fns::objectKeysValuesAll::new(pos, named, expr)?;
      let o = args.o(cx, env)?;
      let fields = o.all_sorted_fields(cx.str_ar);
      let exprs = path_exprs(cx, env.path())?;
      let mut ret = Array::default();
      for (key, field) in fields {
        let (env, expr) = field.into_expr(&mut exprs.ar, env.path());
        let elem = key_value_obj(&mut exprs.ar, key, expr);
        ret.push(env, Some(elem));
      }
      Ok(ret.into())
    }

    StdFn::objectRemoveKey => {
      let args = fns::objectRemoveKey::new(pos, named, expr)?;
      let mut obj = args.obj(cx, env)?;
      let key = args.key(cx, env)?;
      let exprs = path_exprs(cx, env.path())?;
      obj.remove_key_and_asserts(key, &mut exprs.ar, env.path());
      Ok(obj.into())
    }

    StdFn::mapWithKey => {
      let args = fns::mapWithKey::new(pos, named, expr)?;
      let func = args.func(cx, env)?;
      let obj = args.obj(cx, env)?;
      exec::ck_object_asserts(cx, &obj)?;
      let mut ret = cx.obj_mk.mk(Env::empty(env.path()), Vec::new(), ExprFields::new());
      for (key, mut arg_env, val) in obj.visible_fields() {
        let func = Some(lift_val(cx, &mut arg_env, Val::Fn(func.clone()))?);
        let exprs = path_exprs(cx, arg_env.path())?;
        let key_expr = Some(exprs.ar.alloc(ExprData::Prim(Prim::String(key))));
        let expr = Some(exprs.ar.alloc(ExprData::Call {
          func,
          positional: vec![key_expr, val],
          named: Vec::new(),
        }));
        // use default vis
        let field = ExprField { vis: Vis::Default, expr, comp_subst: None };
        let fields = ExprFields::from([(key, field)]);
        let obj = cx.obj_mk.mk(arg_env, Vec::new(), fields);
        // inefficient. makes each field its own one-field object in a + chain
        ret.set_parent_to(obj);
      }
      Ok(ret.into())
    }

    StdFn::prune => {
      let args = fns::prune::new(pos, named, expr)?;
      let arg = args.a(cx, env)?;
      let ret = prune(cx, env.path(), arg.clone())?;
      // unwrap_or is for prune(null), prune({}), prune([])
      Ok(ret.unwrap_or(arg))
    }

    StdFn::exponent => {
      // TODO impl `exponent`
      Err(mk_todo(expr, "exponent"))
    }

    StdFn::mantissa => {
      // TODO impl `mantissa`
      Err(mk_todo(expr, "mantissa"))
    }

    StdFn::mod_ => {
      // TODO impl `mod_`
      Err(mk_todo(expr, "mod_"))
    }

    StdFn::toString => {
      // TODO impl `toString`
      Err(mk_todo(expr, "toString"))
    }

    StdFn::codepoint => {
      // TODO impl `codepoint`
      Err(mk_todo(expr, "codepoint"))
    }

    StdFn::char => {
      // TODO impl `char`
      Err(mk_todo(expr, "char"))
    }

    StdFn::findSubstr => {
      // TODO impl `findSubstr`
      Err(mk_todo(expr, "findSubstr"))
    }

    StdFn::split => {
      // TODO impl `split`
      Err(mk_todo(expr, "split"))
    }

    StdFn::splitLimit => {
      // TODO impl `splitLimit`
      Err(mk_todo(expr, "splitLimit"))
    }

    StdFn::splitLimitR => {
      // TODO impl `splitLimitR`
      Err(mk_todo(expr, "splitLimitR"))
    }

    StdFn::trim => {
      // TODO impl `trim`
      Err(mk_todo(expr, "trim"))
    }

    StdFn::stringChars => {
      // TODO impl `stringChars`
      Err(mk_todo(expr, "stringChars"))
    }

    StdFn::escapeStringBash => {
      // TODO impl `escapeStringBash`
      Err(mk_todo(expr, "escapeStringBash"))
    }

    StdFn::escapeStringDollars => {
      // TODO impl `escapeStringDollars`
      Err(mk_todo(expr, "escapeStringDollars"))
    }

    StdFn::escapeStringJson => {
      // TODO impl `escapeStringJson`
      Err(mk_todo(expr, "escapeStringJson"))
    }

    StdFn::escapeStringPython => {
      // TODO impl `escapeStringPython`
      Err(mk_todo(expr, "escapeStringPython"))
    }

    StdFn::escapeStringXml => {
      // TODO impl `escapeStringXml`
      Err(mk_todo(expr, "escapeStringXml"))
    }

    StdFn::parseInt => {
      // TODO impl `parseInt`
      Err(mk_todo(expr, "parseInt"))
    }

    StdFn::parseOctal => {
      // TODO impl `parseOctal`
      Err(mk_todo(expr, "parseOctal"))
    }

    StdFn::parseHex => {
      // TODO impl `parseHex`
      Err(mk_todo(expr, "parseHex"))
    }

    StdFn::parseJson => {
      // TODO impl `parseJson`
      Err(mk_todo(expr, "parseJson"))
    }

    StdFn::parseYaml => {
      // TODO impl `parseYaml`
      Err(mk_todo(expr, "parseYaml"))
    }

    StdFn::encodeUTF8 => {
      // TODO impl `encodeUTF8`
      Err(mk_todo(expr, "encodeUTF8"))
    }

    StdFn::decodeUTF8 => {
      // TODO impl `decodeUTF8`
      Err(mk_todo(expr, "decodeUTF8"))
    }

    StdFn::manifestIni => {
      // TODO impl `manifestIni`
      Err(mk_todo(expr, "manifestIni"))
    }

    StdFn::manifestPython => {
      // TODO impl `manifestPython`
      Err(mk_todo(expr, "manifestPython"))
    }

    StdFn::manifestPythonVars => {
      // TODO impl `manifestPythonVars`
      Err(mk_todo(expr, "manifestPythonVars"))
    }

    StdFn::manifestJsonEx => {
      // TODO impl `manifestJsonEx`
      Err(mk_todo(expr, "manifestJsonEx"))
    }

    StdFn::manifestJson => {
      // TODO impl `manifestJson`
      Err(mk_todo(expr, "manifestJson"))
    }

    StdFn::manifestJsonMinified => {
      // TODO impl `manifestJsonMinified`
      Err(mk_todo(expr, "manifestJsonMinified"))
    }

    StdFn::manifestYamlDoc => {
      // TODO impl `manifestYamlDoc`
      Err(mk_todo(expr, "manifestYamlDoc"))
    }

    StdFn::manifestYamlStream => {
      // TODO impl `manifestYamlStream`
      Err(mk_todo(expr, "manifestYamlStream"))
    }

    StdFn::manifestXmlJsonml => {
      // TODO impl `manifestXmlJsonml`
      Err(mk_todo(expr, "manifestXmlJsonml"))
    }

    StdFn::manifestTomlEx => {
      // TODO impl `manifestTomlEx`
      Err(mk_todo(expr, "manifestTomlEx"))
    }

    StdFn::member => {
      // TODO impl `member`
      Err(mk_todo(expr, "member"))
    }

    StdFn::count => {
      // TODO impl `count`
      Err(mk_todo(expr, "count"))
    }

    StdFn::find => {
      // TODO impl `find`
      Err(mk_todo(expr, "find"))
    }

    StdFn::map => {
      // TODO impl `map`
      Err(mk_todo(expr, "map"))
    }

    StdFn::mapWithIndex => {
      // TODO impl `mapWithIndex`
      Err(mk_todo(expr, "mapWithIndex"))
    }

    StdFn::filterMap => {
      // TODO impl `filterMap`
      Err(mk_todo(expr, "filterMap"))
    }

    StdFn::flatMap => {
      // TODO impl `flatMap`
      Err(mk_todo(expr, "flatMap"))
    }

    StdFn::filter => {
      // TODO impl `filter`
      Err(mk_todo(expr, "filter"))
    }

    StdFn::foldl => {
      // TODO impl `foldl`
      Err(mk_todo(expr, "foldl"))
    }

    StdFn::foldr => {
      // TODO impl `foldr`
      Err(mk_todo(expr, "foldr"))
    }

    StdFn::range => {
      // TODO impl `range`
      Err(mk_todo(expr, "range"))
    }

    StdFn::repeat => {
      // TODO impl `repeat`
      Err(mk_todo(expr, "repeat"))
    }

    StdFn::slice => {
      // TODO impl `slice`
      Err(mk_todo(expr, "slice"))
    }

    StdFn::deepJoin => {
      // TODO impl `deepJoin`
      Err(mk_todo(expr, "deepJoin"))
    }

    StdFn::lines => {
      // TODO impl `lines`
      Err(mk_todo(expr, "lines"))
    }

    StdFn::flattenArrays => {
      // TODO impl `flattenArrays`
      Err(mk_todo(expr, "flattenArrays"))
    }

    StdFn::flattenDeepArray => {
      // TODO impl `flattenDeepArray`
      Err(mk_todo(expr, "flattenDeepArray"))
    }

    StdFn::reverse => {
      // TODO impl `reverse`
      Err(mk_todo(expr, "reverse"))
    }

    StdFn::sort => {
      // TODO impl `sort`
      Err(mk_todo(expr, "sort"))
    }

    StdFn::uniq => {
      // TODO impl `uniq`
      Err(mk_todo(expr, "uniq"))
    }

    StdFn::all => {
      // TODO impl `all`
      Err(mk_todo(expr, "all"))
    }

    StdFn::any => {
      // TODO impl `any`
      Err(mk_todo(expr, "any"))
    }

    StdFn::sum => {
      // TODO impl `sum`
      Err(mk_todo(expr, "sum"))
    }

    StdFn::avg => {
      // TODO impl `avg`
      Err(mk_todo(expr, "avg"))
    }

    StdFn::minArray => {
      // TODO impl `minArray`
      Err(mk_todo(expr, "minArray"))
    }

    StdFn::maxArray => {
      // TODO impl `maxArray`
      Err(mk_todo(expr, "maxArray"))
    }

    StdFn::contains => {
      // TODO impl `contains`
      Err(mk_todo(expr, "contains"))
    }

    StdFn::remove => {
      // TODO impl `remove`
      Err(mk_todo(expr, "remove"))
    }

    StdFn::removeAt => {
      // TODO impl `removeAt`
      Err(mk_todo(expr, "removeAt"))
    }

    StdFn::set => {
      // TODO impl `set`
      Err(mk_todo(expr, "set"))
    }

    StdFn::setInter => {
      // TODO impl `setInter`
      Err(mk_todo(expr, "setInter"))
    }

    StdFn::setUnion => {
      // TODO impl `setUnion`
      Err(mk_todo(expr, "setUnion"))
    }

    StdFn::setDiff => {
      // TODO impl `setDiff`
      Err(mk_todo(expr, "setDiff"))
    }

    StdFn::setMember => {
      // TODO impl `setMember`
      Err(mk_todo(expr, "setMember"))
    }

    StdFn::base64 => {
      // TODO impl `base64`
      Err(mk_todo(expr, "base64"))
    }

    StdFn::base64DecodeBytes => {
      // TODO impl `base64DecodeBytes`
      Err(mk_todo(expr, "base64DecodeBytes"))
    }

    StdFn::base64Decode => {
      // TODO impl `base64Decode`
      Err(mk_todo(expr, "base64Decode"))
    }

    StdFn::md5 => {
      // TODO impl `md5`
      Err(mk_todo(expr, "md5"))
    }

    StdFn::sha1 => {
      // TODO impl `sha1`
      Err(mk_todo(expr, "sha1"))
    }

    StdFn::sha256 => {
      // TODO impl `sha256`
      Err(mk_todo(expr, "sha256"))
    }

    StdFn::sha3 => {
      // TODO impl `sha3`
      Err(mk_todo(expr, "sha3"))
    }

    StdFn::sha512 => {
      // TODO impl `sha512`
      Err(mk_todo(expr, "sha512"))
    }

    StdFn::mergePatch => {
      // TODO impl `mergePatch`
      Err(mk_todo(expr, "mergePatch"))
    }

    StdFn::trace => {
      // TODO impl `trace`
      Err(mk_todo(expr, "trace"))
    }

    StdFn::native => {
      // TODO impl `native`
      Err(mk_todo(expr, "native"))
    }

    StdFn::extVar => {
      // TODO impl `extVar`
      Err(mk_todo(expr, "extVar"))
    }
  }
}

fn path_exprs<'a>(cx: &'a mut Cx<'_>, path: paths::PathId) -> Result<&'a mut crate::Exprs> {
  if let Some(exprs) = cx.exprs.get_mut(&path) {
    Ok(exprs)
  } else {
    always!(false, "should have this paths's exprs");
    Err(Error::NoPath(path))
  }
}

fn key_value_obj(ar: &mut ExprArena, key: Str, val: Expr) -> ExprMust {
  let key_str = Some(ar.alloc(ExprData::Prim(Prim::String(Str::key))));
  let val_str = Some(ar.alloc(ExprData::Prim(Prim::String(Str::value))));
  let k = Some(ar.alloc(ExprData::Prim(Prim::String(key))));
  let fields = vec![
    jsonnet_expr::Field { key: key_str, vis: jsonnet_expr::Vis::Default, val: k },
    jsonnet_expr::Field { key: val_str, vis: jsonnet_expr::Vis::Default, val },
  ];
  ar.alloc(ExprData::Object { asserts: Vec::new(), fields })
}

/// Mutates `env` and returns an expr that evaluates to `val` in that `env`.
fn lift_val(cx: &mut Cx<'_>, env: &mut Env, val: Val) -> Result<ExprMust> {
  let func_id = cx.str_ar.id_fresh_unutterable();
  let exprs = path_exprs(cx, env.path())?;
  env.insert(Subst { id: func_id, val: ValOrExpr::Val(val) });
  Ok(exprs.ar.alloc(ExprData::Id(func_id)))
}

fn prune(cx: &mut Cx<'_>, path: paths::PathId, val: Val) -> Result<Option<Val>> {
  match val {
    Val::Prim(prim) => match prim {
      Prim::Null => Ok(None),
      Prim::Bool(_) | Prim::String(_) | Prim::Number(_) => Ok(Some(Val::Prim(prim))),
    },
    Val::Object(object) => {
      let fields = object.sorted_visible_fields(cx.str_ar);
      if fields.is_empty() {
        return Ok(None);
      }
      let mut new_env = Env::empty(path);
      let mut new_fields = ExprFields::new();
      for (key, env, field) in fields {
        let val = exec::get(cx, &env, field)?;
        let Some(val) = prune(cx, path, val)? else { continue };
        let expr = Some(lift_val(cx, &mut new_env, val)?);
        let new_field = ExprField { vis: Vis::Default, expr, comp_subst: None };
        new_fields.insert(key, new_field);
      }
      let new_obj = cx.obj_mk.mk(new_env, Vec::new(), new_fields);
      Ok(Some(Val::Object(new_obj)))
    }
    Val::Array(array) => {
      if array.is_empty() {
        return Ok(None);
      }
      let mut new_env = Env::empty(path);
      let mut new_elems = Vec::new();
      for (env, elem) in array.elems() {
        let val = exec::get(cx, env, elem)?;
        let Some(val) = prune(cx, path, val)? else { continue };
        let new_elem = Some(lift_val(cx, &mut new_env, val)?);
        new_elems.push(new_elem);
      }
      let new_array = Array::new(new_env, new_elems);
      Ok(Some(Val::Array(new_array)))
    }
    Val::Fn(f) => Ok(Some(Val::Fn(f))),
  }
}
