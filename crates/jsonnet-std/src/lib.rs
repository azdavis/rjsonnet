//! The names and parameter names of the std lib fns.

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
  /// A number like `0` or `123` or `-456`.
  Num,
  /// A string like `"foo"` or `"bar"` or `""`.
  Str,
  /// An array of booleans, like `[false, true]`.
  ArrBool,
  /// An array of numbers, like `[1, 4]`.
  ArrNum,
  /// An array of strings, like `["hi", "bye"]`.
  ArrStr,
  /// An array with any contents, like `["hi", 3, null, false]`.
  ArrAny,
  /// An object with arbitrary fields like `{ foo: 3 }` or `{}`.
  Obj,
  /// A string or an array of numbers.
  StrOrArrNum,
}

/// `r` for "required"
const fn r(name: &'static str, ty: Ty) -> Param {
  Param { name, ty, required: true }
}

/// `o` for "optional"
const fn o(name: &'static str, ty: Ty) -> Param {
  Param { name, ty, required: false }
}

/// `s` for "sig"
const fn s(params: &'static [Param], ret: Ty) -> Sig {
  Sig { params, ret }
}

const V_ANY_RET_BOOL: Sig = s(&[r("v", Ty::Any)], Ty::Bool);
const X_NUM_RET_NUM: Sig = s(&[r("x", Ty::Num)], Ty::Num);
const N_NUM_RET_NUM: Sig = s(&[r("n", Ty::Num)], Ty::Num);
const X_NUM_RET_BOOL: Sig = s(&[r("x", Ty::Num)], Ty::Bool);
const STR_RET_STR: Sig = s(&[r("str", Ty::Str)], Ty::Str);
const X_Y_BOOL_RET_BOOL: Sig = s(&[r("x", Ty::Bool), r("y", Ty::Bool)], Ty::Bool);
const A_B_STR_RET_BOOL: Sig = s(&[r("a", Ty::Str), r("b", Ty::Str)], Ty::Bool);
const STR_CHARS_STR_RET_STR: Sig = s(&[r("str", Ty::Str), r("chars", Ty::Str)], Ty::Str);
const STR_RET_NUM: Sig = s(&[r("str", Ty::Str)], Ty::Num);
const STR_RET_ANY: Sig = s(&[r("str", Ty::Str)], Ty::Any);
const SPLIT_LIMIT: Sig =
  s(&[r("str", Ty::Str), r("c", Ty::Str), r("maxsplits", Ty::Str)], Ty::ArrStr);
const OBJ_HAS: Sig = s(&[r("o", Ty::Obj), r("f", Ty::Str)], Ty::Bool);
const OBJ_FIELDS: Sig = s(&[r("o", Ty::Obj)], Ty::ArrStr);
const OBJ_VALUES: Sig = s(&[r("o", Ty::Obj)], Ty::ArrAny);
const MANIFEST_JSON: Sig = s(&[r("value", Ty::Any)], Ty::Str);
const MAP: Sig = s(&[r("func", Ty::Any), r("arr", Ty::ArrAny)], Ty::ArrAny);
const FOLD: Sig = s(&[r("func", Ty::Any), r("arr", Ty::ArrAny), r("init", Ty::Any)], Ty::Any);
const ARR_KEY_F: Sig = s(&[r("arr", Ty::ArrAny), o("keyF", Ty::Any)], Ty::ArrAny);
const BINARY_SET_FN: Sig =
  s(&[r("a", Ty::ArrAny), r("b", Ty::ArrAny), o("keyF", Ty::Any)], Ty::ArrAny);

const fn f(name: &'static str, sig: Sig) -> Fn {
  Fn { name: S::new(name), sig }
}

/// The std fns.
pub const FNS: [Fn; 126] = [
  f("extVar", s(&[r("x", Ty::Str)], Ty::Str)),
  Fn { name: S::named("type", "type_"), sig: s(&[r("x", Ty::Any)], Ty::Str) },
  f("isArray", V_ANY_RET_BOOL),
  f("isBoolean", V_ANY_RET_BOOL),
  f("isFunction", V_ANY_RET_BOOL),
  f("isNumber", V_ANY_RET_BOOL),
  f("isObject", V_ANY_RET_BOOL),
  f("isString", V_ANY_RET_BOOL),
  f("length", s(&[r("x", Ty::Any)], Ty::Num)),
  f(
    "get",
    s(
      &[r("o", Ty::Obj), r("f", Ty::Str), o("default", Ty::Any), o("inc_hidden", Ty::Bool)],
      Ty::Any,
    ),
  ),
  f("objectHas", OBJ_HAS),
  f("objectFields", OBJ_FIELDS),
  f("objectValues", OBJ_VALUES),
  f("objectKeysValues", s(&[r("o", Ty::Obj)], Ty::ArrAny)),
  f("objectHasAll", OBJ_HAS),
  f("objectFieldsAll", OBJ_FIELDS),
  f("objectValuesAll", OBJ_VALUES),
  f("objectKeysValuesAll", OBJ_VALUES),
  f("prune", s(&[r("a", Ty::Any)], Ty::Any)),
  f("mapWithKey", s(&[r("func", Ty::Any), r("obj", Ty::Obj)], Ty::Obj)),
  f("abs", N_NUM_RET_NUM),
  f("sign", N_NUM_RET_NUM),
  f("max", s(&[r("a", Ty::Num), r("b", Ty::Num)], Ty::Num)),
  f("min", s(&[r("a", Ty::Num), r("b", Ty::Num)], Ty::Num)),
  f("pow", s(&[r("x", Ty::Num), r("n", Ty::Num)], Ty::Num)),
  f("exp", X_NUM_RET_NUM),
  f("log", X_NUM_RET_NUM),
  f("exponent", X_NUM_RET_NUM),
  f("mantissa", X_NUM_RET_NUM),
  f("floor", X_NUM_RET_NUM),
  f("ceil", X_NUM_RET_NUM),
  f("sqrt", X_NUM_RET_NUM),
  f("sin", X_NUM_RET_NUM),
  f("cos", X_NUM_RET_NUM),
  f("tan", X_NUM_RET_NUM),
  f("asin", X_NUM_RET_NUM),
  f("acos", X_NUM_RET_NUM),
  f("atan", X_NUM_RET_NUM),
  f("round", X_NUM_RET_NUM),
  f("isEven", X_NUM_RET_BOOL),
  f("isOdd", X_NUM_RET_BOOL),
  f("isInteger", X_NUM_RET_BOOL),
  f("isDecimal", X_NUM_RET_BOOL),
  Fn { name: S::named("mod", "mod_"), sig: s(&[r("a", Ty::Any), r("b", Ty::Any)], Ty::Any) },
  f("clamp", s(&[r("x", Ty::Num), r("minVal", Ty::Num), r("maxVal", Ty::Num)], Ty::Num)),
  f("assertEqual", s(&[r("a", Ty::Any), r("b", Ty::Any)], Ty::True)),
  f("toString", s(&[r("a", Ty::Any)], Ty::Str)),
  f("codepoint", s(&[r("str", Ty::Str)], Ty::Num)),
  f("char", s(&[r("n", Ty::Num)], Ty::Str)),
  f("substr", s(&[r("str", Ty::Str), r("from", Ty::Num), r("len", Ty::Num)], Ty::Str)),
  f("findSubstr", s(&[r("pat", Ty::Str), r("str", Ty::Str)], Ty::ArrNum)),
  f("startsWith", A_B_STR_RET_BOOL),
  f("endsWith", A_B_STR_RET_BOOL),
  f("stripChars", STR_CHARS_STR_RET_STR),
  f("lstripChars", STR_CHARS_STR_RET_STR),
  f("rstripChars", STR_CHARS_STR_RET_STR),
  f("split", s(&[r("str", Ty::Str), r("c", Ty::Str)], Ty::ArrStr)),
  f("splitLimit", SPLIT_LIMIT),
  f("splitLimitR", SPLIT_LIMIT),
  f("strReplace", s(&[r("str", Ty::Str), r("from", Ty::Str), r("to", Ty::Str)], Ty::Str)),
  f("isEmpty", s(&[r("str", Ty::Str)], Ty::Bool)),
  f("asciiUpper", STR_RET_STR),
  f("asciiLower", STR_RET_STR),
  f("stringChars", STR_RET_STR),
  f("format", s(&[r("str", Ty::Str), r("vals", Ty::ArrAny)], Ty::Str)),
  f("escapeStringBash", STR_RET_STR),
  f("escapeStringDollars", STR_RET_STR),
  f("escapeStringJson", STR_RET_STR),
  f("escapeStringPython", STR_RET_STR),
  f("escapeStringXml", STR_RET_STR),
  f("parseInt", STR_RET_NUM),
  f("parseOctal", STR_RET_NUM),
  f("parseHex", STR_RET_NUM),
  f("parseJson", STR_RET_ANY),
  f("parseYaml", STR_RET_ANY),
  f("encodeUTF8", s(&[r("str", Ty::Str)], Ty::ArrNum)),
  f("decodeUTF8", s(&[r("arr", Ty::ArrNum)], Ty::Str)),
  f("manifestIni", s(&[r("ini", Ty::Obj)], Ty::Str)),
  f("manifestPython", s(&[r("v", Ty::Any)], Ty::Str)),
  f("manifestPythonVars", s(&[r("conf", Ty::Any)], Ty::Str)),
  f(
    "manifestJsonEx",
    s(
      &[
        r("value", Ty::Any),
        r("indent", Ty::Str),
        r("newline", Ty::Str),
        r("key_val_sep", Ty::Str),
      ],
      Ty::Str,
    ),
  ),
  f("manifestJson", MANIFEST_JSON),
  f("manifestJsonMinified", MANIFEST_JSON),
  f(
    "manifestYamlDoc",
    s(
      &[r("value", Ty::Any), o("indent_array_in_object", Ty::Bool), o("quote_keys", Ty::Bool)],
      Ty::Str,
    ),
  ),
  f(
    "manifestYamlStream",
    s(
      &[
        r("value", Ty::ArrAny),
        o("indent_array_in_object", Ty::Bool),
        o("c_document_end", Ty::Bool),
        o("quote_keys", Ty::Bool),
      ],
      Ty::Str,
    ),
  ),
  f("manifestXmlJsonml", s(&[r("value", Ty::ArrAny)], Ty::Str)),
  f("manifestTomlEx", s(&[r("toml", Ty::Obj), r("indent", Ty::Str)], Ty::Str)),
  f("makeArray", s(&[r("sz", Ty::Num), r("func", Ty::Any)], Ty::ArrAny)),
  f("member", s(&[r("arr", Ty::Any), r("x", Ty::Any)], Ty::Bool)),
  f("count", s(&[r("arr", Ty::ArrAny), r("x", Ty::Any)], Ty::Num)),
  f("find", s(&[r("value", Ty::Any), r("arr", Ty::ArrAny)], Ty::ArrNum)),
  f("map", MAP),
  f("mapWithIndex", MAP),
  f(
    "filterMap",
    s(&[r("filter_func", Ty::Any), r("map_func", Ty::Any), r("arr", Ty::ArrAny)], Ty::ArrAny),
  ),
  f("flatMap", MAP),
  f("filter", MAP),
  f("foldl", FOLD),
  f("foldr", FOLD),
  f("range", s(&[r("from", Ty::Num), r("to", Ty::Num)], Ty::Num)),
  f("repeat", s(&[r("what", Ty::Any), r("count", Ty::Num)], Ty::Any)),
  f(
    "slice",
    s(
      &[r("indexable", Ty::Any), r("index", Ty::Num), r("end", Ty::Num), r("step", Ty::Num)],
      Ty::Any,
    ),
  ),
  f("join", s(&[r("sep", Ty::Any), r("arr", Ty::Any)], Ty::Any)),
  f("lines", s(&[r("arr", Ty::ArrStr)], Ty::Str)),
  f("flattenArrays", s(&[r("arr", Ty::ArrAny)], Ty::ArrAny)),
  f("reverse", s(&[r("arr", Ty::ArrAny)], Ty::ArrAny)),
  f("sort", ARR_KEY_F),
  f("uniq", ARR_KEY_F),
  f("all", s(&[r("arr", Ty::ArrBool)], Ty::Bool)),
  f("any", s(&[r("arr", Ty::ArrBool)], Ty::Bool)),
  f("sum", s(&[r("arr", Ty::ArrNum)], Ty::Num)),
  f("avg", s(&[r("arr", Ty::ArrNum)], Ty::Num)),
  f("set", ARR_KEY_F),
  f("setInter", BINARY_SET_FN),
  f("setUnion", BINARY_SET_FN),
  f("setDiff", BINARY_SET_FN),
  f("setMember", s(&[r("x", Ty::Any), r("arr", Ty::ArrAny), o("keyF", Ty::Any)], Ty::Bool)),
  f("base64", s(&[r("input", Ty::StrOrArrNum)], Ty::Str)),
  f("base64DecodeBytes", s(&[r("str", Ty::Str)], Ty::ArrNum)),
  f("base64Decode", STR_RET_STR),
  f("md5", s(&[r("s", Ty::Str)], Ty::Str)),
  f("xor", X_Y_BOOL_RET_BOOL),
  f("xnor", X_Y_BOOL_RET_BOOL),
  f("mergePatch", s(&[r("target", Ty::Any), r("patch", Ty::Any)], Ty::Any)),
  f("trace", s(&[r("str", Ty::Str), r("rest", Ty::Any)], Ty::Any)),
  // alluded to in the spec but not mentioned on the std lib page
  f("equals", s(&[r("x", Ty::Any), r("y", Ty::Any)], Ty::Bool)),
  f("objectHasEx", s(&[r("obj", Ty::Obj), r("fname", Ty::Str), r("hidden", Ty::Bool)], Ty::Bool)),
];
