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
pub enum Sig {
  /// A simple signature, expressible in a simple type system. It has parameters and a return type.
  Simple(&'static [Param], Ty),
  /// A complex signature with custom type handling.
  Complex(&'static [&'static str]),
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

const V_ANY_RET_BOOL: Sig = Sig::Simple(&[r("v", Ty::Any)], Ty::Bool);
const X_NUM_RET_NUM: Sig = Sig::Simple(&[r("x", Ty::Num)], Ty::Num);
const N_NUM_RET_NUM: Sig = Sig::Simple(&[r("n", Ty::Num)], Ty::Num);
const X_NUM_RET_BOOL: Sig = Sig::Simple(&[r("x", Ty::Num)], Ty::Bool);
const STR_RET_STR: Sig = Sig::Simple(&[r("str", Ty::Str)], Ty::Str);
const X_Y_BOOL_RET_BOOL: Sig = Sig::Simple(&[r("x", Ty::Bool), r("y", Ty::Bool)], Ty::Bool);
const A_B_STR_RET_BOOL: Sig = Sig::Simple(&[r("a", Ty::Str), r("b", Ty::Str)], Ty::Bool);
const STR_CHARS_STR_RET_STR: Sig = Sig::Simple(&[r("str", Ty::Str), r("chars", Ty::Str)], Ty::Str);
const STR_RET_NUM: Sig = Sig::Simple(&[r("str", Ty::Str)], Ty::Num);
const STR_RET_ANY: Sig = Sig::Simple(&[r("str", Ty::Str)], Ty::Any);
const SPLIT_LIMIT: Sig =
  Sig::Simple(&[r("str", Ty::Str), r("c", Ty::Str), r("maxsplits", Ty::Str)], Ty::ArrStr);
const OBJ_HAS: Sig = Sig::Simple(&[r("o", Ty::Obj), r("f", Ty::Str)], Ty::Bool);
const OBJ_FIELDS: Sig = Sig::Simple(&[r("o", Ty::Obj)], Ty::ArrStr);
const MANIFEST_JSON: Sig = Sig::Simple(&[r("value", Ty::Any)], Ty::Str);

const fn f(name: &'static str, sig: Sig) -> Fn {
  Fn { name: S::new(name), sig }
}

/// The std fns.
pub const FNS: [Fn; 126] = [
  f("extVar", Sig::Simple(&[r("x", Ty::Str)], Ty::Str)),
  Fn { name: S::named("type", "type_"), sig: Sig::Simple(&[r("x", Ty::Any)], Ty::Str) },
  f("isArray", V_ANY_RET_BOOL),
  f("isBoolean", V_ANY_RET_BOOL),
  f("isFunction", V_ANY_RET_BOOL),
  f("isNumber", V_ANY_RET_BOOL),
  f("isObject", V_ANY_RET_BOOL),
  f("isString", V_ANY_RET_BOOL),
  f("length", Sig::Complex(&["x"])),
  f(
    "get",
    Sig::Simple(
      &[r("o", Ty::Obj), r("f", Ty::Str), o("default", Ty::Any), o("inc_hidden", Ty::Bool)],
      Ty::Any,
    ),
  ),
  f("objectHas", OBJ_HAS),
  f("objectFields", OBJ_FIELDS),
  f("objectValues", Sig::Complex(&["o"])),
  f("objectKeysValues", Sig::Complex(&["o"])),
  f("objectHasAll", OBJ_HAS),
  f("objectFieldsAll", OBJ_FIELDS),
  f("objectValuesAll", Sig::Complex(&["o"])),
  f("objectKeysValuesAll", Sig::Complex(&["o"])),
  f("prune", Sig::Complex(&["a"])),
  f("mapWithKey", Sig::Complex(&["func", "obj"])),
  f("abs", N_NUM_RET_NUM),
  f("sign", N_NUM_RET_NUM),
  f("max", Sig::Simple(&[r("a", Ty::Num), r("b", Ty::Num)], Ty::Num)),
  f("min", Sig::Simple(&[r("a", Ty::Num), r("b", Ty::Num)], Ty::Num)),
  f("pow", Sig::Simple(&[r("x", Ty::Num), r("n", Ty::Num)], Ty::Num)),
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
  Fn { name: S::named("mod", "mod_"), sig: Sig::Complex(&["a", "b"]) },
  f("clamp", Sig::Simple(&[r("x", Ty::Num), r("minVal", Ty::Num), r("maxVal", Ty::Num)], Ty::Num)),
  f("assertEqual", Sig::Complex(&["a", "b"])),
  f("toString", Sig::Simple(&[r("a", Ty::Any)], Ty::Str)),
  f("codepoint", Sig::Simple(&[r("str", Ty::Str)], Ty::Num)),
  f("char", Sig::Simple(&[r("n", Ty::Num)], Ty::Str)),
  f("substr", Sig::Simple(&[r("str", Ty::Str), r("from", Ty::Num), r("len", Ty::Num)], Ty::Str)),
  f("findSubstr", Sig::Simple(&[r("pat", Ty::Str), r("str", Ty::Str)], Ty::ArrNum)),
  f("startsWith", A_B_STR_RET_BOOL),
  f("endsWith", A_B_STR_RET_BOOL),
  f("stripChars", STR_CHARS_STR_RET_STR),
  f("lstripChars", STR_CHARS_STR_RET_STR),
  f("rstripChars", STR_CHARS_STR_RET_STR),
  f("split", Sig::Simple(&[r("str", Ty::Str), r("c", Ty::Str)], Ty::ArrStr)),
  f("splitLimit", SPLIT_LIMIT),
  f("splitLimitR", SPLIT_LIMIT),
  f("strReplace", Sig::Simple(&[r("str", Ty::Str), r("from", Ty::Str), r("to", Ty::Str)], Ty::Str)),
  f("isEmpty", Sig::Simple(&[r("str", Ty::Str)], Ty::Bool)),
  f("asciiUpper", STR_RET_STR),
  f("asciiLower", STR_RET_STR),
  f("stringChars", STR_RET_STR),
  f("format", Sig::Complex(&["str", "vals"])),
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
  f("encodeUTF8", Sig::Simple(&[r("str", Ty::Str)], Ty::ArrNum)),
  f("decodeUTF8", Sig::Simple(&[r("arr", Ty::ArrNum)], Ty::Str)),
  f("manifestIni", Sig::Simple(&[r("ini", Ty::Obj)], Ty::Str)),
  f("manifestPython", Sig::Simple(&[r("v", Ty::Any)], Ty::Str)),
  f("manifestPythonVars", Sig::Simple(&[r("conf", Ty::Any)], Ty::Str)),
  f(
    "manifestJsonEx",
    Sig::Simple(
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
    Sig::Simple(
      &[r("value", Ty::Any), o("indent_array_in_object", Ty::Bool), o("quote_keys", Ty::Bool)],
      Ty::Str,
    ),
  ),
  f(
    "manifestYamlStream",
    Sig::Simple(
      &[
        r("value", Ty::ArrAny),
        o("indent_array_in_object", Ty::Bool),
        o("c_document_end", Ty::Bool),
        o("quote_keys", Ty::Bool),
      ],
      Ty::Str,
    ),
  ),
  f("manifestXmlJsonml", Sig::Simple(&[r("value", Ty::ArrAny)], Ty::Str)),
  f("manifestTomlEx", Sig::Simple(&[r("toml", Ty::Obj), r("indent", Ty::Str)], Ty::Str)),
  f("makeArray", Sig::Complex(&["sz", "func"])),
  f("member", Sig::Complex(&["arr", "x"])),
  f("count", Sig::Complex(&["arr", "x"])),
  f("find", Sig::Complex(&["value", "arr"])),
  f("map", Sig::Complex(&["func", "arr"])),
  f("mapWithIndex", Sig::Complex(&["func", "arr"])),
  f("filterMap", Sig::Complex(&["filter_func", "map_func", "arr"])),
  f("flatMap", Sig::Complex(&["func", "arr"])),
  f("filter", Sig::Complex(&["func", "arr"])),
  f("foldl", Sig::Complex(&["func", "arr", "init"])),
  f("foldr", Sig::Complex(&["func", "arr", "init"])),
  f("range", Sig::Simple(&[r("from", Ty::Num), r("to", Ty::Num)], Ty::Num)),
  f("repeat", Sig::Complex(&["what", "count"])),
  f("slice", Sig::Complex(&["indexable", "index", "end", "step"])),
  f("join", Sig::Complex(&["sep", "arr"])),
  f("lines", Sig::Simple(&[r("arr", Ty::ArrStr)], Ty::Str)),
  f("flattenArrays", Sig::Complex(&["arr"])),
  f("reverse", Sig::Complex(&["arr"])),
  f("sort", Sig::Complex(&["arr", "keyF"])),
  f("uniq", Sig::Complex(&["arr", "keyF"])),
  f("all", Sig::Simple(&[r("arr", Ty::ArrBool)], Ty::Bool)),
  f("any", Sig::Simple(&[r("arr", Ty::ArrBool)], Ty::Bool)),
  f("sum", Sig::Simple(&[r("arr", Ty::ArrNum)], Ty::Num)),
  f("avg", Sig::Simple(&[r("arr", Ty::ArrNum)], Ty::Num)),
  f("set", Sig::Complex(&["arr", "keyF"])),
  f("setInter", Sig::Complex(&["a", "b", "keyF"])),
  f("setUnion", Sig::Complex(&["a", "b", "keyF"])),
  f("setDiff", Sig::Complex(&["a", "b", "keyF"])),
  f("setMember", Sig::Complex(&["x", "arr", "keyF"])),
  f("base64", Sig::Simple(&[r("input", Ty::StrOrArrNum)], Ty::Str)),
  f("base64DecodeBytes", Sig::Simple(&[r("str", Ty::Str)], Ty::ArrNum)),
  f("base64Decode", STR_RET_STR),
  f("md5", Sig::Simple(&[r("s", Ty::Str)], Ty::Str)),
  f("xor", X_Y_BOOL_RET_BOOL),
  f("xnor", X_Y_BOOL_RET_BOOL),
  f("mergePatch", Sig::Complex(&["target", "patch"])),
  f("trace", Sig::Complex(&["str", "rest"])),
  // alluded to in the spec but not mentioned on the std lib page
  f("equals", Sig::Complex(&["x", "y"])),
  f(
    "objectHasEx",
    Sig::Simple(&[r("obj", Ty::Obj), r("fname", Ty::Str), r("hidden", Ty::Bool)], Ty::Bool),
  ),
];
