//! The standard library for Jsonnet, implemented in Rust.

use crate::error::Result;
use crate::{error, exec, mk_todo, Cx};
use finite_float::Float;
use jsonnet_expr::{std_fn, Expr, ExprMust, Id, Prim, StdFn, Str};
use jsonnet_val::jsonnet::{Array, Env, Fn, Val};

pub(crate) fn get(
  cx: Cx<'_>,
  env: &Env,
  positional: &[Expr],
  named: &[(Id, Expr)],
  expr: ExprMust,
  std_fn: StdFn,
) -> Result<Val> {
  match std_fn {
    StdFn::extVar => {
      let _ = std_fn::args::extVar(positional, named, expr)?;
      Err(mk_todo(expr, "std.extVar"))
    }
    StdFn::type_ => {
      let arguments = std_fn::args::type_(positional, named, expr)?;
      let x = exec::get(cx, env, arguments.x)?;
      let ret = match x {
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
      Ok(Val::Prim(Prim::String(ret)))
    }
    StdFn::isArray => {
      let arguments = std_fn::args::isArray(positional, named, expr)?;
      let v = exec::get(cx, env, arguments.v)?;
      Ok(Val::Prim(Prim::Bool(matches!(v, Val::Array(_)))))
    }
    StdFn::isBoolean => {
      let arguments = std_fn::args::isBoolean(positional, named, expr)?;
      let v = exec::get(cx, env, arguments.v)?;
      Ok(Val::Prim(Prim::Bool(matches!(v, Val::Prim(Prim::Bool(_))))))
    }
    StdFn::isFunction => {
      let arguments = std_fn::args::isFunction(positional, named, expr)?;
      let v = exec::get(cx, env, arguments.v)?;
      Ok(Val::Prim(Prim::Bool(matches!(v, Val::Fn(_)))))
    }
    StdFn::isNumber => {
      let arguments = std_fn::args::isNumber(positional, named, expr)?;
      let v = exec::get(cx, env, arguments.v)?;
      Ok(Val::Prim(Prim::Bool(matches!(v, Val::Prim(Prim::Number(_))))))
    }
    StdFn::isObject => {
      let arguments = std_fn::args::isObject(positional, named, expr)?;
      let v = exec::get(cx, env, arguments.v)?;
      Ok(Val::Prim(Prim::Bool(matches!(v, Val::Object(_)))))
    }
    StdFn::isString => {
      let arguments = std_fn::args::isString(positional, named, expr)?;
      let v = exec::get(cx, env, arguments.v)?;
      Ok(Val::Prim(Prim::Bool(matches!(v, Val::Prim(Prim::String(_))))))
    }
    StdFn::length => {
      let arguments = std_fn::args::length(positional, named, expr)?;
      let x = exec::get(cx, env, arguments.x)?;
      let ret = match x {
        Val::Prim(prim) => match prim {
          Prim::Null | Prim::Bool(_) | Prim::Number(_) => {
            return Err(error::Error::Exec {
              expr: arguments.x.unwrap_or(expr),
              kind: error::Kind::IncompatibleTypes,
            });
          }
          // we want "number of codepoints", NOT byte length.
          Prim::String(s) => cx.str_ar.get(&s).chars().count(),
        },
        Val::Object(obj) => obj.fields().len(),
        Val::Array(arr) => arr.len(),
        Val::Fn(Fn::Regular(func)) => func.params.iter().filter(|(_, d)| d.is_none()).count(),
        Val::Fn(Fn::Std(_)) => return Err(mk_todo(expr, "std.length of std function")),
      };
      Ok(Val::Prim(Prim::Number(Float::from(ret))))
    }
    StdFn::get => {
      let _ = std_fn::args::get(positional, named, expr)?;
      Err(mk_todo(expr, "std.get"))
    }
    StdFn::objectHas => {
      let _ = std_fn::args::objectHas(positional, named, expr)?;
      Err(mk_todo(expr, "std.objectHas"))
    }
    StdFn::objectFields => {
      let _ = std_fn::args::objectFields(positional, named, expr)?;
      Err(mk_todo(expr, "std.objectFields"))
    }
    StdFn::objectValues => {
      let _ = std_fn::args::objectValues(positional, named, expr)?;
      Err(mk_todo(expr, "std.objectValues"))
    }
    StdFn::objectKeysValues => {
      let _ = std_fn::args::objectKeysValues(positional, named, expr)?;
      Err(mk_todo(expr, "std.objectKeysValues"))
    }
    StdFn::objectHasAll => {
      let _ = std_fn::args::objectHasAll(positional, named, expr)?;
      Err(mk_todo(expr, "std.objectHasAll"))
    }
    StdFn::objectFieldsAll => {
      let _ = std_fn::args::objectFieldsAll(positional, named, expr)?;
      Err(mk_todo(expr, "std.objectFieldsAll"))
    }
    StdFn::objectValuesAll => {
      let _ = std_fn::args::objectValuesAll(positional, named, expr)?;
      Err(mk_todo(expr, "std.objectValuesAll"))
    }
    StdFn::objectKeysValuesAll => {
      let _ = std_fn::args::objectKeysValuesAll(positional, named, expr)?;
      Err(mk_todo(expr, "std.objectKeysValuesAll"))
    }
    StdFn::prune => {
      let _ = std_fn::args::prune(positional, named, expr)?;
      Err(mk_todo(expr, "std.prune"))
    }
    StdFn::mapWithKey => {
      let _ = std_fn::args::mapWithKey(positional, named, expr)?;
      Err(mk_todo(expr, "std.mapWithKey"))
    }
    StdFn::abs => math_op(cx, env, positional, named, expr, f64::abs),
    StdFn::sign => {
      let arguments = std_fn::args::sign(positional, named, expr)?;
      let n = exec::get(cx, env, arguments.n)?;
      let val = get_num(&n, arguments.n.unwrap_or(expr))?;
      let res = if val == 0.0 {
        Float::positive_zero()
      } else if val.is_sign_positive() {
        Float::positive_one()
      } else {
        Float::negative_one()
      };
      Ok(Val::Prim(Prim::Number(res)))
    }
    StdFn::max => {
      let arguments = std_fn::args::max(positional, named, expr)?;
      let a = exec::get(cx, env, arguments.a)?;
      let b = exec::get(cx, env, arguments.b)?;
      let a = get_num(&a, arguments.a.unwrap_or(expr))?;
      let b = get_num(&b, arguments.b.unwrap_or(expr))?;
      let res = a.max(b);
      mk_num(res, expr)
    }
    StdFn::min => {
      let arguments = std_fn::args::min(positional, named, expr)?;
      let a = exec::get(cx, env, arguments.a)?;
      let b = exec::get(cx, env, arguments.b)?;
      let a = get_num(&a, arguments.a.unwrap_or(expr))?;
      let b = get_num(&b, arguments.b.unwrap_or(expr))?;
      let res = a.min(b);
      mk_num(res, expr)
    }
    StdFn::pow => {
      let arguments = std_fn::args::pow(positional, named, expr)?;
      let x = exec::get(cx, env, arguments.x)?;
      let n = exec::get(cx, env, arguments.n)?;
      let x = get_num(&x, arguments.x.unwrap_or(expr))?;
      let n = get_num(&n, arguments.n.unwrap_or(expr))?;
      let res = x.powf(n);
      mk_num(res, expr)
    }
    StdFn::exp => math_op(cx, env, positional, named, expr, f64::exp),
    // TODO is it log2 or log10?
    StdFn::log => math_op(cx, env, positional, named, expr, f64::log2),
    StdFn::exponent => {
      let _ = std_fn::args::exponent(positional, named, expr)?;
      Err(mk_todo(expr, "std.exponent"))
    }
    StdFn::mantissa => {
      let _ = std_fn::args::mantissa(positional, named, expr)?;
      Err(mk_todo(expr, "std.mantissa"))
    }
    StdFn::floor => math_op(cx, env, positional, named, expr, f64::floor),
    StdFn::ceil => math_op(cx, env, positional, named, expr, f64::ceil),
    StdFn::sqrt => math_op(cx, env, positional, named, expr, f64::sqrt),
    StdFn::sin => math_op(cx, env, positional, named, expr, f64::sin),
    StdFn::cos => math_op(cx, env, positional, named, expr, f64::cos),
    StdFn::tan => math_op(cx, env, positional, named, expr, f64::tan),
    StdFn::asin => math_op(cx, env, positional, named, expr, f64::asin),
    StdFn::acos => math_op(cx, env, positional, named, expr, f64::acos),
    StdFn::atan => math_op(cx, env, positional, named, expr, f64::atan),
    StdFn::round => math_op(cx, env, positional, named, expr, f64::round),
    StdFn::isEven => {
      let _ = std_fn::args::isEven(positional, named, expr)?;
      Err(mk_todo(expr, "std.isEven"))
    }
    StdFn::isOdd => {
      let _ = std_fn::args::isOdd(positional, named, expr)?;
      Err(mk_todo(expr, "std.isOdd"))
    }
    StdFn::isInteger => {
      let _ = std_fn::args::isInteger(positional, named, expr)?;
      Err(mk_todo(expr, "std.isInteger"))
    }
    StdFn::isDecimal => {
      let _ = std_fn::args::isDecimal(positional, named, expr)?;
      Err(mk_todo(expr, "std.isDecimal"))
    }
    StdFn::mod_ => {
      let _ = std_fn::args::mod_(positional, named, expr)?;
      Err(mk_todo(expr, "std.mod"))
    }
    StdFn::clamp => {
      let _ = std_fn::args::clamp(positional, named, expr)?;
      Err(mk_todo(expr, "std.clamp"))
    }
    StdFn::assertEqual => {
      let _ = std_fn::args::assertEqual(positional, named, expr)?;
      Err(mk_todo(expr, "std.assertEqual"))
    }
    StdFn::toString => {
      let _ = std_fn::args::toString(positional, named, expr)?;
      Err(mk_todo(expr, "std.toString"))
    }
    StdFn::codepoint => {
      let _ = std_fn::args::codepoint(positional, named, expr)?;
      Err(mk_todo(expr, "std.codepoint"))
    }
    StdFn::char => {
      let _ = std_fn::args::char(positional, named, expr)?;
      Err(mk_todo(expr, "std.char"))
    }
    StdFn::substr => {
      let _ = std_fn::args::substr(positional, named, expr)?;
      Err(mk_todo(expr, "std.substr"))
    }
    StdFn::findSubstr => {
      let _ = std_fn::args::findSubstr(positional, named, expr)?;
      Err(mk_todo(expr, "std.findSubstr"))
    }
    StdFn::startsWith => {
      let _ = std_fn::args::startsWith(positional, named, expr)?;
      Err(mk_todo(expr, "std.startsWith"))
    }
    StdFn::endsWith => {
      let _ = std_fn::args::endsWith(positional, named, expr)?;
      Err(mk_todo(expr, "std.endsWith"))
    }
    StdFn::stripChars => {
      let _ = std_fn::args::stripChars(positional, named, expr)?;
      Err(mk_todo(expr, "std.stripChars"))
    }
    StdFn::lstripChars => {
      let _ = std_fn::args::lstripChars(positional, named, expr)?;
      Err(mk_todo(expr, "std.lstripChars"))
    }
    StdFn::rstripChars => {
      let _ = std_fn::args::rstripChars(positional, named, expr)?;
      Err(mk_todo(expr, "std.rstripChars"))
    }
    StdFn::split => {
      let _ = std_fn::args::split(positional, named, expr)?;
      Err(mk_todo(expr, "std.split"))
    }
    StdFn::splitLimit => {
      let _ = std_fn::args::splitLimit(positional, named, expr)?;
      Err(mk_todo(expr, "std.splitLimit"))
    }
    StdFn::splitLimitR => {
      let _ = std_fn::args::splitLimitR(positional, named, expr)?;
      Err(mk_todo(expr, "std.splitLimitR"))
    }
    StdFn::strReplace => {
      let _ = std_fn::args::strReplace(positional, named, expr)?;
      Err(mk_todo(expr, "std.strReplace"))
    }
    StdFn::isEmpty => {
      let _ = std_fn::args::isEmpty(positional, named, expr)?;
      Err(mk_todo(expr, "std.isEmpty"))
    }
    StdFn::asciiUpper => {
      let _ = std_fn::args::asciiUpper(positional, named, expr)?;
      Err(mk_todo(expr, "std.asciiUpper"))
    }
    StdFn::asciiLower => {
      let _ = std_fn::args::asciiLower(positional, named, expr)?;
      Err(mk_todo(expr, "std.asciiLower"))
    }
    StdFn::stringChars => {
      let _ = std_fn::args::stringChars(positional, named, expr)?;
      Err(mk_todo(expr, "std.stringChars"))
    }
    StdFn::format => {
      let _ = std_fn::args::format(positional, named, expr)?;
      Err(mk_todo(expr, "std.format"))
    }
    StdFn::escapeStringBash => {
      let _ = std_fn::args::escapeStringBash(positional, named, expr)?;
      Err(mk_todo(expr, "std.escapeStringBash"))
    }
    StdFn::escapeStringDollars => {
      let _ = std_fn::args::escapeStringDollars(positional, named, expr)?;
      Err(mk_todo(expr, "std.escapeStringDollars"))
    }
    StdFn::escapeStringJson => {
      let _ = std_fn::args::escapeStringJson(positional, named, expr)?;
      Err(mk_todo(expr, "std.escapeStringJson"))
    }
    StdFn::escapeStringPython => {
      let _ = std_fn::args::escapeStringPython(positional, named, expr)?;
      Err(mk_todo(expr, "std.escapeStringPython"))
    }
    StdFn::escapeStringXml => {
      let _ = std_fn::args::escapeStringXml(positional, named, expr)?;
      Err(mk_todo(expr, "std.escapeStringXml"))
    }
    StdFn::parseInt => {
      let _ = std_fn::args::parseInt(positional, named, expr)?;
      Err(mk_todo(expr, "std.parseInt"))
    }
    StdFn::parseOctal => {
      let _ = std_fn::args::parseOctal(positional, named, expr)?;
      Err(mk_todo(expr, "std.parseOctal"))
    }
    StdFn::parseHex => {
      let _ = std_fn::args::parseHex(positional, named, expr)?;
      Err(mk_todo(expr, "std.parseHex"))
    }
    StdFn::parseJson => {
      let _ = std_fn::args::parseJson(positional, named, expr)?;
      Err(mk_todo(expr, "std.parseJson"))
    }
    StdFn::parseYaml => {
      let _ = std_fn::args::parseYaml(positional, named, expr)?;
      Err(mk_todo(expr, "std.parseYaml"))
    }
    StdFn::encodeUTF8 => {
      let _ = std_fn::args::encodeUTF8(positional, named, expr)?;
      Err(mk_todo(expr, "std.encodeUTF8"))
    }
    StdFn::decodeUTF8 => {
      let _ = std_fn::args::decodeUTF8(positional, named, expr)?;
      Err(mk_todo(expr, "std.decodeUTF8"))
    }
    StdFn::manifestIni => {
      let _ = std_fn::args::manifestIni(positional, named, expr)?;
      Err(mk_todo(expr, "std.manifestIni"))
    }
    StdFn::manifestPython => {
      let _ = std_fn::args::manifestPython(positional, named, expr)?;
      Err(mk_todo(expr, "std.manifestPython"))
    }
    StdFn::manifestPythonVars => {
      let _ = std_fn::args::manifestPythonVars(positional, named, expr)?;
      Err(mk_todo(expr, "std.manifestPythonVars"))
    }
    StdFn::manifestJsonEx => {
      let _ = std_fn::args::manifestJsonEx(positional, named, expr)?;
      Err(mk_todo(expr, "std.manifestJsonEx"))
    }
    StdFn::manifestJson => {
      let _ = std_fn::args::manifestJson(positional, named, expr)?;
      Err(mk_todo(expr, "std.manifestJson"))
    }
    StdFn::manifestJsonMinified => {
      let _ = std_fn::args::manifestJsonMinified(positional, named, expr)?;
      Err(mk_todo(expr, "std.manifestJsonMinified"))
    }
    StdFn::manifestYamlDoc => {
      let _ = std_fn::args::manifestYamlDoc(positional, named, expr)?;
      Err(mk_todo(expr, "std.manifestYamlDoc"))
    }
    StdFn::manifestYamlStream => {
      let _ = std_fn::args::manifestYamlStream(positional, named, expr)?;
      Err(mk_todo(expr, "std.manifestYamlStream"))
    }
    StdFn::manifestXmlJsonml => {
      let _ = std_fn::args::manifestXmlJsonml(positional, named, expr)?;
      Err(mk_todo(expr, "std.manifestXmlJsonml"))
    }
    StdFn::manifestTomlEx => {
      let _ = std_fn::args::manifestTomlEx(positional, named, expr)?;
      Err(mk_todo(expr, "std.manifestTomlEx"))
    }
    StdFn::makeArray => {
      let _ = std_fn::args::makeArray(positional, named, expr)?;
      Err(mk_todo(expr, "std.makeArray"))
    }
    StdFn::member => {
      let _ = std_fn::args::member(positional, named, expr)?;
      Err(mk_todo(expr, "std.member"))
    }
    StdFn::count => {
      let _ = std_fn::args::count(positional, named, expr)?;
      Err(mk_todo(expr, "std.count"))
    }
    StdFn::find => {
      let _ = std_fn::args::find(positional, named, expr)?;
      Err(mk_todo(expr, "std.find"))
    }
    StdFn::map => {
      let _ = std_fn::args::map(positional, named, expr)?;
      Err(mk_todo(expr, "std.map"))
    }
    StdFn::mapWithIndex => {
      let _ = std_fn::args::mapWithIndex(positional, named, expr)?;
      Err(mk_todo(expr, "std.mapWithIndex"))
    }
    StdFn::filterMap => {
      let _ = std_fn::args::filterMap(positional, named, expr)?;
      Err(mk_todo(expr, "std.filterMap"))
    }
    StdFn::flatMap => {
      let _ = std_fn::args::flatMap(positional, named, expr)?;
      Err(mk_todo(expr, "std.flatMap"))
    }
    StdFn::filter => {
      let _ = std_fn::args::filter(positional, named, expr)?;
      Err(mk_todo(expr, "std.filter"))
    }
    StdFn::foldl => {
      let _ = std_fn::args::foldl(positional, named, expr)?;
      Err(mk_todo(expr, "std.foldl"))
    }
    StdFn::foldr => {
      let _ = std_fn::args::foldr(positional, named, expr)?;
      Err(mk_todo(expr, "std.foldr"))
    }
    StdFn::range => {
      let _ = std_fn::args::range(positional, named, expr)?;
      Err(mk_todo(expr, "std.range"))
    }
    StdFn::repeat => {
      let _ = std_fn::args::repeat(positional, named, expr)?;
      Err(mk_todo(expr, "std.repeat"))
    }
    StdFn::slice => {
      let _ = std_fn::args::slice(positional, named, expr)?;
      Err(mk_todo(expr, "std.slice"))
    }
    StdFn::join => {
      let arguments = std_fn::args::join(positional, named, expr)?;
      let sep = exec::get(cx, env, arguments.sep)?;
      let Val::Array(arr) = exec::get(cx, env, arguments.arr)? else {
        return Err(error::Error::Exec {
          expr: arguments.arr.unwrap_or(expr),
          kind: error::Kind::IncompatibleTypes,
        });
      };
      match sep {
        Val::Prim(Prim::String(sep)) => {
          let mut ret = String::new();
          let sep = cx.str_ar.get(&sep);
          let mut first = true;
          for (env, elem) in arr.iter() {
            if !first {
              ret.push_str(sep);
            };
            first = false;
            let Val::Prim(Prim::String(elem)) = exec::get(cx, env, elem)? else {
              return Err(error::Error::Exec {
                expr: elem.unwrap_or(expr),
                kind: error::Kind::IncompatibleTypes,
              });
            };
            ret.push_str(cx.str_ar.get(&elem));
          }
          Ok(Val::Prim(Prim::String(cx.str_ar.str_shared(ret.into_boxed_str()))))
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
    StdFn::lines => {
      let _ = std_fn::args::lines(positional, named, expr)?;
      Err(mk_todo(expr, "std.lines"))
    }
    StdFn::flattenArrays => {
      let _ = std_fn::args::flattenArrays(positional, named, expr)?;
      Err(mk_todo(expr, "std.flattenArrays"))
    }
    StdFn::reverse => {
      let _ = std_fn::args::reverse(positional, named, expr)?;
      Err(mk_todo(expr, "std.reverse"))
    }
    StdFn::sort => {
      let _ = std_fn::args::sort(positional, named, expr)?;
      Err(mk_todo(expr, "std.sort"))
    }
    StdFn::uniq => {
      let _ = std_fn::args::uniq(positional, named, expr)?;
      Err(mk_todo(expr, "std.uniq"))
    }
    StdFn::all => {
      let _ = std_fn::args::all(positional, named, expr)?;
      Err(mk_todo(expr, "std.all"))
    }
    StdFn::any => {
      let _ = std_fn::args::any(positional, named, expr)?;
      Err(mk_todo(expr, "std.any"))
    }
    StdFn::sum => {
      let _ = std_fn::args::sum(positional, named, expr)?;
      Err(mk_todo(expr, "std.sum"))
    }
    StdFn::avg => {
      let _ = std_fn::args::avg(positional, named, expr)?;
      Err(mk_todo(expr, "std.avg"))
    }
    StdFn::set => {
      let _ = std_fn::args::set(positional, named, expr)?;
      Err(mk_todo(expr, "std.set"))
    }
    StdFn::setInter => {
      let _ = std_fn::args::setInter(positional, named, expr)?;
      Err(mk_todo(expr, "std.setInter"))
    }
    StdFn::setUnion => {
      let _ = std_fn::args::setUnion(positional, named, expr)?;
      Err(mk_todo(expr, "std.setUnion"))
    }
    StdFn::setDiff => {
      let _ = std_fn::args::setDiff(positional, named, expr)?;
      Err(mk_todo(expr, "std.setDiff"))
    }
    StdFn::setMember => {
      let _ = std_fn::args::setMember(positional, named, expr)?;
      Err(mk_todo(expr, "std.setMember"))
    }
    StdFn::base64 => {
      let _ = std_fn::args::base64(positional, named, expr)?;
      Err(mk_todo(expr, "std.base64"))
    }
    StdFn::base64DecodeBytes => {
      let _ = std_fn::args::base64DecodeBytes(positional, named, expr)?;
      Err(mk_todo(expr, "std.base64DecodeBytes"))
    }
    StdFn::base64Decode => {
      let _ = std_fn::args::base64Decode(positional, named, expr)?;
      Err(mk_todo(expr, "std.base64Decode"))
    }
    StdFn::md5 => {
      let _ = std_fn::args::md5(positional, named, expr)?;
      Err(mk_todo(expr, "std.md5"))
    }
    StdFn::xor => {
      let _ = std_fn::args::xor(positional, named, expr)?;
      Err(mk_todo(expr, "std.xor"))
    }
    StdFn::xnor => {
      let _ = std_fn::args::xnor(positional, named, expr)?;
      Err(mk_todo(expr, "std.xnor"))
    }
    StdFn::mergePatch => {
      let _ = std_fn::args::mergePatch(positional, named, expr)?;
      Err(mk_todo(expr, "std.mergePatch"))
    }
    StdFn::trace => {
      let _ = std_fn::args::trace(positional, named, expr)?;
      Err(mk_todo(expr, "std.trace"))
    }
    StdFn::equals => {
      let arguments = std_fn::args::equals(positional, named, expr)?;
      let lhs = exec::get(cx, env, arguments.x)?;
      let rhs = exec::get(cx, env, arguments.y)?;
      Ok(Val::Prim(Prim::Bool(exec::eq_val(expr, cx, &lhs, &rhs)?)))
    }
    StdFn::objectHasEx => {
      let _ = std_fn::args::objectHasEx(positional, named, expr)?;
      Err(mk_todo(expr, "std.objectHasEx"))
    }
  }
}

fn math_op(
  cx: Cx<'_>,
  env: &Env,
  positional: &[Expr],
  named: &[(Id, Expr)],
  expr: ExprMust,
  f: fn(f64) -> f64,
) -> Result<Val> {
  let arguments = std_fn::params::x::get(positional, named, expr)?;
  let x = exec::get(cx, env, arguments.x)?;
  let x = get_num(&x, arguments.x.unwrap_or(expr))?;
  mk_num(f(x), expr)
}

fn get_num(v: &Val, expr: ExprMust) -> Result<f64> {
  match v {
    Val::Prim(Prim::Number(x)) => Ok(x.value()),
    _ => Err(error::Error::Exec { expr, kind: error::Kind::IncompatibleTypes }),
  }
}

fn mk_num(n: f64, expr: ExprMust) -> Result<Val> {
  match Float::try_from(n) {
    Ok(x) => Ok(Val::Prim(Prim::Number(x))),
    Err(e) => Err(error::Error::Exec { expr, kind: error::Kind::Infinite(e) }),
  }
}
