//! The names and parameter names of the standard library functions.

use indoc::indoc;

/// A name-content string pair.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct S {
  /// The name.
  name: &'static str,
  /// The content.
  content: &'static str,
}

impl S {
  /// Make a new k-v pair (actually the param order is v first then k).
  #[must_use]
  pub const fn named(content: &'static str, name: &'static str) -> S {
    S { name, content }
  }

  /// Make a new one whose name (k) is the content (v).
  #[must_use]
  pub const fn new(content: &'static str) -> S {
    S { name: content, content }
  }

  /// Returns the identifier. Must be a valid Rust identifier.
  #[must_use]
  pub const fn ident(&self) -> &'static str {
    self.name
  }

  /// Returns the content. Can be an arbitrary string, including whitespace.
  #[must_use]
  pub const fn content(&self) -> &'static str {
    self.content
  }
}

/// A standard library function.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Fn {
  /// The name.
  pub name: S,
  /// The signature.
  pub sig: Sig,
  /// Whether the function returns a value for all well-typed inputs.
  pub total: bool,
  /// The documentation.
  pub doc: &'static str,
}

/// A signature for a std fn.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Sig {
  /// The params.
  pub params: &'static [Param],
  /// The return type.
  pub ret: Ty,
}

/// A function parameter.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Param {
  /// Its name.
  pub name: &'static str,
  /// Its type.
  pub ty: Ty,
  /// Whether it is required.
  pub required: bool,
}

/// A simple type.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Ty {
  /// Anything at all.
  Any,
  /// Exactly `true`.
  True,
  /// Either `true` or `false`.
  Bool,
  /// A number like `0` or `123` or `-456.789`.
  Num,
  /// A non-negative integer.
  Uint,
  /// A string like `"foo"` or `"bar"` or `""`.
  Str,
  /// A string that is known statically.
  StaticStr,
  /// An array with any contents, like `["hi", 3, null, false]`.
  ArrAny,
  /// An array of booleans, like `[false, true]`.
  ArrBool,
  /// An array of numbers, like `[1, 4]`.
  ArrNum,
  /// An array of strings, like `["hi", "bye"]`.
  ArrStr,
  /// An array of `{ key: string, value: any }`.
  ArrKv,
  /// An object with arbitrary fields.
  Obj,
  /// A string or an array of anything.
  StrOrArrAny,
  /// A string or an array of numbers.
  StrOrArrNum,
  /// A number or `null`.
  NumOrNull,
  /// A number or a string.
  NumOrStr,
  /// A HOF with 1 param.
  Hof1,
  /// A HOF with 2 params.
  Hof2,
}

const fn req(name: &'static str, ty: Ty) -> Param {
  Param { name, ty, required: true }
}

const fn opt(name: &'static str, ty: Ty) -> Param {
  Param { name, ty, required: false }
}

const fn sig(params: &'static [Param], ret: Ty) -> Sig {
  Sig { params, ret }
}

const V_ANY_RET_BOOL: Sig = sig(&[req("v", Ty::Any)], Ty::Bool);
const X_NUM_RET_NUM: Sig = sig(&[req("x", Ty::Num)], Ty::Num);
const N_NUM_RET_NUM: Sig = sig(&[req("n", Ty::Num)], Ty::Num);
const X_NUM_RET_BOOL: Sig = sig(&[req("x", Ty::Num)], Ty::Bool);
const STR_RET_STR: Sig = sig(&[req("str", Ty::Str)], Ty::Str);
const X_Y_BOOL_RET_BOOL: Sig = sig(&[req("x", Ty::Bool), req("y", Ty::Bool)], Ty::Bool);
const A_B_STR_RET_BOOL: Sig = sig(&[req("a", Ty::Str), req("b", Ty::Str)], Ty::Bool);
const STR_CHARS_STR_RET_STR: Sig = sig(&[req("str", Ty::Str), req("chars", Ty::Str)], Ty::Str);
const STR_RET_NUM: Sig = sig(&[req("str", Ty::Str)], Ty::Num);
const STR_RET_ANY: Sig = sig(&[req("str", Ty::Str)], Ty::Any);
const SPLIT_LIMIT: Sig =
  sig(&[req("str", Ty::Str), req("c", Ty::Str), req("maxsplits", Ty::Num)], Ty::ArrStr);
const OBJ_HAS: Sig = sig(&[req("o", Ty::Obj), req("f", Ty::Str)], Ty::Bool);
const OBJ_FIELDS: Sig = sig(&[req("o", Ty::Obj)], Ty::ArrStr);
const OBJ_VALUES: Sig = sig(&[req("o", Ty::Obj)], Ty::ArrAny);
const OBJ_KEYS_VALUES: Sig = sig(&[req("o", Ty::Obj)], Ty::ArrKv);
const MANIFEST_JSON: Sig = sig(&[req("value", Ty::Any)], Ty::Str);
const ARR_HOF1: Sig = sig(&[req("func", Ty::Hof1), req("arr", Ty::ArrAny)], Ty::ArrAny);
const FOLD: Sig =
  sig(&[req("func", Ty::Hof2), req("arr", Ty::ArrAny), req("init", Ty::Any)], Ty::Any);
const ARR_KEY_F: Sig = sig(&[req("arr", Ty::ArrAny), opt("keyF", Ty::Hof1)], Ty::ArrAny);
const BINARY_SET_FN: Sig =
  sig(&[req("a", Ty::ArrAny), req("b", Ty::ArrAny), opt("keyF", Ty::Hof1)], Ty::ArrAny);

/// The std fns.
pub const FNS: [Fn; 126] = [
  Fn {
    name: S::new("extVar"),
    sig: sig(&[req("x", Ty::Str)], Ty::Str),
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::named("type", "type_"),
    sig: sig(&[req("x", Ty::Any)], Ty::StaticStr),
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("isArray"),
    sig: V_ANY_RET_BOOL,
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("isBoolean"),
    sig: V_ANY_RET_BOOL,
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("isFunction"),
    sig: V_ANY_RET_BOOL,
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("isNumber"),
    sig: V_ANY_RET_BOOL,
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("isObject"),
    sig: V_ANY_RET_BOOL,
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("isString"),
    sig: V_ANY_RET_BOOL,
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("length"),
    sig: sig(&[req("x", Ty::Any)], Ty::Uint),
    total: false,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("get"),
    sig: sig(
      &[req("o", Ty::Obj), req("f", Ty::Str), opt("default", Ty::Any), opt("inc_hidden", Ty::Bool)],
      Ty::Any,
    ),
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("objectHas"),
    sig: OBJ_HAS,
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("objectFields"),
    sig: OBJ_FIELDS,
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("objectValues"),
    sig: OBJ_VALUES,
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("objectKeysValues"),
    sig: OBJ_KEYS_VALUES,
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("objectHasAll"),
    sig: OBJ_HAS,
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("objectFieldsAll"),
    sig: OBJ_FIELDS,
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("objectValuesAll"),
    sig: OBJ_VALUES,
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("objectKeysValuesAll"),
    sig: OBJ_KEYS_VALUES,
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("prune"),
    sig: sig(&[req("a", Ty::Any)], Ty::Any),
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("mapWithKey"),
    sig: sig(&[req("func", Ty::Hof2), req("obj", Ty::Obj)], Ty::Obj),
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("abs"),
    sig: N_NUM_RET_NUM,
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("sign"),
    sig: N_NUM_RET_NUM,
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("max"),
    sig: sig(&[req("a", Ty::Num), req("b", Ty::Num)], Ty::Num),
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("min"),
    sig: sig(&[req("a", Ty::Num), req("b", Ty::Num)], Ty::Num),
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("pow"),
    sig: sig(&[req("x", Ty::Num), req("n", Ty::Num)], Ty::Num),
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("exp"),
    sig: X_NUM_RET_NUM,
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("log"),
    sig: X_NUM_RET_NUM,
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("exponent"),
    sig: X_NUM_RET_NUM,
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("mantissa"),
    sig: X_NUM_RET_NUM,
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("floor"),
    sig: X_NUM_RET_NUM,
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("ceil"),
    sig: X_NUM_RET_NUM,
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("sqrt"),
    sig: X_NUM_RET_NUM,
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("sin"),
    sig: X_NUM_RET_NUM,
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("cos"),
    sig: X_NUM_RET_NUM,
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("tan"),
    sig: X_NUM_RET_NUM,
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("asin"),
    sig: X_NUM_RET_NUM,
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("acos"),
    sig: X_NUM_RET_NUM,
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("atan"),
    sig: X_NUM_RET_NUM,
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("round"),
    sig: X_NUM_RET_NUM,
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("isEven"),
    sig: X_NUM_RET_BOOL,
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("isOdd"),
    sig: X_NUM_RET_BOOL,
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("isInteger"),
    sig: X_NUM_RET_BOOL,
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("isDecimal"),
    sig: X_NUM_RET_BOOL,
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::named("mod", "mod_"),
    sig: sig(&[req("a", Ty::NumOrStr), req("b", Ty::Any)], Ty::Any),
    total: false,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("clamp"),
    sig: sig(&[req("x", Ty::Num), req("minVal", Ty::Num), req("maxVal", Ty::Num)], Ty::Num),
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("assertEqual"),
    sig: sig(&[req("a", Ty::Any), req("b", Ty::Any)], Ty::True),
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("toString"),
    sig: sig(&[req("a", Ty::Any)], Ty::Str),
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("codepoint"),
    sig: sig(&[req("str", Ty::Str)], Ty::Uint),
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("char"),
    sig: sig(&[req("n", Ty::Uint)], Ty::Str),
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("substr"),
    sig: sig(&[req("str", Ty::Str), req("from", Ty::Uint), req("len", Ty::Uint)], Ty::Str),
    total: false,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("findSubstr"),
    sig: sig(&[req("pat", Ty::Str), req("str", Ty::Str)], Ty::ArrNum),
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("startsWith"),
    sig: A_B_STR_RET_BOOL,
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("endsWith"),
    sig: A_B_STR_RET_BOOL,
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("stripChars"),
    sig: STR_CHARS_STR_RET_STR,
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("lstripChars"),
    sig: STR_CHARS_STR_RET_STR,
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("rstripChars"),
    sig: STR_CHARS_STR_RET_STR,
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("split"),
    sig: sig(&[req("str", Ty::Str), req("c", Ty::Str)], Ty::ArrStr),
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("splitLimit"),
    sig: SPLIT_LIMIT,
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("splitLimitR"),
    sig: SPLIT_LIMIT,
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("strReplace"),
    sig: sig(&[req("str", Ty::Str), req("from", Ty::Str), req("to", Ty::Str)], Ty::Str),
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("isEmpty"),
    sig: sig(&[req("str", Ty::Str)], Ty::Bool),
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("asciiUpper"),
    sig: STR_RET_STR,
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("asciiLower"),
    sig: STR_RET_STR,
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("stringChars"),
    sig: sig(&[req("str", Ty::Str)], Ty::ArrStr),
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("format"),
    sig: sig(&[req("str", Ty::Str), req("vals", Ty::Any)], Ty::Str),
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("escapeStringBash"),
    sig: STR_RET_STR,
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("escapeStringDollars"),
    sig: STR_RET_STR,
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("escapeStringJson"),
    sig: STR_RET_STR,
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("escapeStringPython"),
    sig: STR_RET_STR,
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("escapeStringXml"),
    sig: STR_RET_STR,
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("parseInt"),
    sig: STR_RET_NUM,
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("parseOctal"),
    sig: STR_RET_NUM,
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("parseHex"),
    sig: STR_RET_NUM,
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("parseJson"),
    sig: STR_RET_ANY,
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("parseYaml"),
    sig: STR_RET_ANY,
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("encodeUTF8"),
    sig: sig(&[req("str", Ty::Str)], Ty::ArrNum),
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("decodeUTF8"),
    sig: sig(&[req("arr", Ty::ArrNum)], Ty::Str),
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("manifestIni"),
    sig: sig(&[req("ini", Ty::Obj)], Ty::Str),
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("manifestPython"),
    sig: sig(&[req("v", Ty::Any)], Ty::Str),
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("manifestPythonVars"),
    sig: sig(&[req("conf", Ty::Any)], Ty::Str),
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("manifestJsonEx"),
    sig: sig(
      &[
        req("value", Ty::Any),
        req("indent", Ty::Str),
        opt("newline", Ty::Str),
        opt("key_val_sep", Ty::Str),
      ],
      Ty::Str,
    ),
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("manifestJson"),
    sig: MANIFEST_JSON,
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("manifestJsonMinified"),
    sig: MANIFEST_JSON,
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("manifestYamlDoc"),
    sig: sig(
      &[
        req("value", Ty::Any),
        opt("indent_array_in_object", Ty::Bool),
        opt("quote_keys", Ty::Bool),
      ],
      Ty::Str,
    ),
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("manifestYamlStream"),
    sig: sig(
      &[
        req("value", Ty::ArrAny),
        opt("indent_array_in_object", Ty::Bool),
        opt("c_document_end", Ty::Bool),
        opt("quote_keys", Ty::Bool),
      ],
      Ty::Str,
    ),
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("manifestXmlJsonml"),
    sig: sig(&[req("value", Ty::ArrAny)], Ty::Str),
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("manifestTomlEx"),
    sig: sig(&[req("toml", Ty::Obj), req("indent", Ty::Str)], Ty::Str),
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("makeArray"),
    sig: sig(&[req("sz", Ty::Uint), req("func", Ty::Hof1)], Ty::ArrAny),
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("member"),
    sig: sig(&[req("arr", Ty::StrOrArrAny), req("x", Ty::Any)], Ty::Bool),
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("count"),
    sig: sig(&[req("arr", Ty::ArrAny), req("x", Ty::Any)], Ty::Num),
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("find"),
    sig: sig(&[req("value", Ty::Any), req("arr", Ty::ArrAny)], Ty::ArrNum),
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("map"),
    sig: ARR_HOF1,
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("mapWithIndex"),
    sig: sig(&[req("func", Ty::Hof2), req("arr", Ty::ArrAny)], Ty::ArrAny),
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("filterMap"),
    sig: sig(
      &[req("filter_func", Ty::Hof1), req("map_func", Ty::Hof1), req("arr", Ty::ArrAny)],
      Ty::ArrAny,
    ),
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("flatMap"),
    sig: sig(&[req("func", Ty::Hof1), req("arr", Ty::StrOrArrAny)], Ty::StrOrArrAny),
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("filter"),
    sig: ARR_HOF1,
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("foldl"),
    sig: FOLD,
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("foldr"),
    sig: FOLD,
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("range"),
    sig: sig(&[req("from", Ty::Num), req("to", Ty::Num)], Ty::ArrNum),
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("repeat"),
    sig: sig(&[req("what", Ty::StrOrArrAny), req("count", Ty::Uint)], Ty::StrOrArrAny),
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("slice"),
    sig: sig(
      &[
        req("indexable", Ty::StrOrArrAny),
        req("index", Ty::Uint),
        req("end", Ty::NumOrNull),
        req("step", Ty::NumOrNull),
      ],
      Ty::StrOrArrAny,
    ),
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("join"),
    sig: sig(&[req("sep", Ty::StrOrArrAny), req("arr", Ty::ArrAny)], Ty::StrOrArrAny),
    total: false,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("lines"),
    sig: sig(&[req("arr", Ty::ArrStr)], Ty::Str),
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("flattenArrays"),
    sig: sig(&[req("arr", Ty::ArrAny)], Ty::ArrAny),
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("reverse"),
    sig: sig(&[req("arr", Ty::ArrAny)], Ty::ArrAny),
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("sort"),
    sig: ARR_KEY_F,
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("uniq"),
    sig: ARR_KEY_F,
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("all"),
    sig: sig(&[req("arr", Ty::ArrBool)], Ty::Bool),
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("any"),
    sig: sig(&[req("arr", Ty::ArrBool)], Ty::Bool),
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("sum"),
    sig: sig(&[req("arr", Ty::ArrNum)], Ty::Num),
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("avg"),
    sig: sig(&[req("arr", Ty::ArrNum)], Ty::Num),
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("set"),
    sig: ARR_KEY_F,
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("setInter"),
    sig: BINARY_SET_FN,
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("setUnion"),
    sig: BINARY_SET_FN,
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("setDiff"),
    sig: BINARY_SET_FN,
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("setMember"),
    sig: sig(&[req("x", Ty::Any), req("arr", Ty::ArrAny), opt("keyF", Ty::Hof1)], Ty::Bool),
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("base64"),
    sig: sig(&[req("input", Ty::StrOrArrNum)], Ty::Str),
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("base64DecodeBytes"),
    sig: sig(&[req("str", Ty::Str)], Ty::ArrNum),
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("base64Decode"),
    sig: STR_RET_STR,
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("md5"),
    sig: sig(&[req("s", Ty::Str)], Ty::Str),
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("xor"),
    sig: X_Y_BOOL_RET_BOOL,
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("xnor"),
    sig: X_Y_BOOL_RET_BOOL,
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("mergePatch"),
    sig: sig(&[req("target", Ty::Any), req("patch", Ty::Any)], Ty::Any),
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("trace"),
    sig: sig(&[req("str", Ty::Str), req("rest", Ty::Any)], Ty::Any),
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
  // alluded to in the spec but not mentioned on the std lib page
  Fn {
    name: S::new("equals"),
    sig: sig(&[req("x", Ty::Any), req("y", Ty::Any)], Ty::Bool),
    total: false,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("objectHasEx"),
    sig: sig(&[req("obj", Ty::Obj), req("fname", Ty::Str), req("hidden", Ty::Bool)], Ty::Bool),
    total: true,
    doc: indoc! {"
      TODO
    "},
  },
];
