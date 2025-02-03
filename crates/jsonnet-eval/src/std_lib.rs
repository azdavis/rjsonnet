//! The standard library for Jsonnet, implemented in Rust.

#![allow(non_snake_case)]
use crate::error::{self, Error, Result};
use crate::util;
use crate::{exec, generated::fns, mk_todo, Cx};
use finite_float::Float;
use jsonnet_expr::{Expr, ExprMust, Id, Prim, StdFn, Str};
use jsonnet_val::jsonnet::{Array, Env, Fn, Val};
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
            return Err(Error::Exec { expr, kind: error::Kind::IncompatibleTypes })
          }
          // we want "number of codepoints", NOT byte length.
          Prim::String(s) => cx.str_ar.get(s).chars().count(),
        },
        Val::Object(obj) => obj.fields().iter().filter(|(_, f)| !f.is_hidden()).count(),
        Val::Array(arr) => arr.len(),
        Val::Fn(Fn::Regular(func)) => func.params.iter().filter(|(_, d)| d.is_none()).count(),
        Val::Fn(Fn::Std(func)) => func.required_params_count(),
      };
      Ok(Float::from(ret).into())
    }

    StdFn::join => {
      let args = fns::join::new(pos, named, expr)?;
      let arr = args.arr(cx, env)?;
      match args.sep(cx, env)? {
        Val::Prim(Prim::String(sep)) => {
          let mut ret = String::new();
          let sep = cx.str_ar.get(sep).to_owned();
          let mut first = true;
          for (env, elem) in arr.iter() {
            if !first {
              ret.push_str(sep.as_str());
            };
            first = false;
            let Val::Prim(Prim::String(elem)) = exec::get(cx, env, elem)? else {
              return Err(error::Error::Exec {
                expr: elem.unwrap_or(expr),
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
          for (env, elem) in arr.iter() {
            if !first {
              ret.append(&mut sep.clone());
            };
            first = false;
            let Val::Array(mut elem) = exec::get(cx, env, elem)? else {
              return Err(error::Error::Exec {
                expr: elem.unwrap_or(expr),
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
      let from = args.from(cx, env)?;
      let len = args.len(cx, env)?;
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
            return Err(error::Error::Exec { expr, kind: error::Kind::IdxNotUtf8Boundary(len) })
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
      Ok(o.get_field(f).is_some_and(|f| !f.is_hidden()).into())
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
      Ok(o.get_field(f).is_some_and(|f| hidden || !f.is_hidden()).into())
    }

    _ => Err(mk_todo(expr, func.as_static_str())),
  }
}
