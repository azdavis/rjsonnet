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

impl Param {
  const fn new(name: &'static str, ty: Ty) -> Self {
    Self { name, ty, required: true }
  }
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
  /// A boolean array like `[false, true]`.
  BoolArr,
  /// A number array like `[1, 4]`.
  NumArr,
  /// A string array like `["hi", "bye"]`.
  StrArr,
  /// An object with arbitrary fields like `{ foo: 3 }` or `{}`.
  Object,
}

const V_ANY_RET_BOOL: Sig = Sig::Simple(&[Param::new("v", Ty::Any)], Ty::Bool);
const X_NUM_RET_NUM: Sig = Sig::Simple(&[Param::new("x", Ty::Num)], Ty::Num);
const N_NUM_RET_NUM: Sig = Sig::Simple(&[Param::new("n", Ty::Num)], Ty::Num);
const X_NUM_RET_BOOL: Sig = Sig::Simple(&[Param::new("x", Ty::Num)], Ty::Bool);
const STR_RET_STR: Sig = Sig::Simple(&[Param::new("str", Ty::Str)], Ty::Str);
const X_Y_BOOL_RET_BOOL: Sig =
  Sig::Simple(&[Param::new("x", Ty::Bool), Param::new("y", Ty::Bool)], Ty::Bool);
const A_B_STR_RET_BOOL: Sig =
  Sig::Simple(&[Param::new("a", Ty::Str), Param::new("b", Ty::Str)], Ty::Bool);
const STR_CHARS_STR_RET_STR: Sig =
  Sig::Simple(&[Param::new("str", Ty::Str), Param::new("chars", Ty::Str)], Ty::Str);
const STR_RET_NUM: Sig = Sig::Simple(&[Param::new("str", Ty::Str)], Ty::Num);
const STR_RET_ANY: Sig = Sig::Simple(&[Param::new("str", Ty::Str)], Ty::Any);
const SPLIT_LIMIT: Sig = Sig::Simple(
  &[Param::new("str", Ty::Str), Param::new("c", Ty::Str), Param::new("maxsplits", Ty::Str)],
  Ty::StrArr,
);

const fn f(name: &'static str, sig: Sig) -> Fn {
  Fn { name: S::new(name), sig }
}

/// The std fns.
pub const FNS: [Fn; 126] = [
  f("extVar", Sig::Simple(&[Param::new("x", Ty::Str)], Ty::Str)),
  Fn { name: S::named("type", "type_"), sig: Sig::Simple(&[Param::new("x", Ty::Any)], Ty::Str) },
  f("isArray", V_ANY_RET_BOOL),
  f("isBoolean", V_ANY_RET_BOOL),
  f("isFunction", V_ANY_RET_BOOL),
  f("isNumber", V_ANY_RET_BOOL),
  f("isObject", V_ANY_RET_BOOL),
  f("isString", V_ANY_RET_BOOL),
  f("length", Sig::Complex(&["x"])),
  f("get", Sig::Complex(&["o", "f", "default", "inc_hidden"])),
  f("objectHas", Sig::Complex(&["o", "f"])),
  f("objectFields", Sig::Complex(&["o"])),
  f("objectValues", Sig::Complex(&["o"])),
  f("objectKeysValues", Sig::Complex(&["o"])),
  f("objectHasAll", Sig::Complex(&["o", "f"])),
  f("objectFieldsAll", Sig::Complex(&["o"])),
  f("objectValuesAll", Sig::Complex(&["o"])),
  f("objectKeysValuesAll", Sig::Complex(&["o"])),
  f("prune", Sig::Complex(&["a"])),
  f("mapWithKey", Sig::Complex(&["func", "obj"])),
  f("abs", N_NUM_RET_NUM),
  f("sign", N_NUM_RET_NUM),
  f("max", Sig::Simple(&[Param::new("a", Ty::Num), Param::new("b", Ty::Num)], Ty::Num)),
  f("min", Sig::Simple(&[Param::new("a", Ty::Num), Param::new("b", Ty::Num)], Ty::Num)),
  f("pow", Sig::Simple(&[Param::new("x", Ty::Num), Param::new("n", Ty::Num)], Ty::Num)),
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
  f(
    "clamp",
    Sig::Simple(
      &[Param::new("x", Ty::Num), Param::new("minVal", Ty::Num), Param::new("maxVal", Ty::Num)],
      Ty::Num,
    ),
  ),
  f("assertEqual", Sig::Complex(&["a", "b"])),
  f("toString", Sig::Simple(&[Param::new("a", Ty::Any)], Ty::Str)),
  f("codepoint", Sig::Simple(&[Param::new("str", Ty::Str)], Ty::Num)),
  f("char", Sig::Simple(&[Param::new("n", Ty::Num)], Ty::Str)),
  f(
    "substr",
    Sig::Simple(
      &[Param::new("str", Ty::Str), Param::new("from", Ty::Num), Param::new("len", Ty::Num)],
      Ty::Str,
    ),
  ),
  f(
    "findSubstr",
    Sig::Simple(&[Param::new("pat", Ty::Str), Param::new("str", Ty::Str)], Ty::NumArr),
  ),
  f("startsWith", A_B_STR_RET_BOOL),
  f("endsWith", A_B_STR_RET_BOOL),
  f("stripChars", STR_CHARS_STR_RET_STR),
  f("lstripChars", STR_CHARS_STR_RET_STR),
  f("rstripChars", STR_CHARS_STR_RET_STR),
  f("split", Sig::Simple(&[Param::new("str", Ty::Str), Param::new("c", Ty::Str)], Ty::StrArr)),
  f("splitLimit", SPLIT_LIMIT),
  f("splitLimitR", SPLIT_LIMIT),
  f(
    "strReplace",
    Sig::Simple(
      &[Param::new("str", Ty::Str), Param::new("from", Ty::Str), Param::new("to", Ty::Str)],
      Ty::Str,
    ),
  ),
  f("isEmpty", Sig::Simple(&[Param::new("str", Ty::Str)], Ty::Bool)),
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
  f("encodeUTF8", Sig::Simple(&[Param::new("str", Ty::Str)], Ty::NumArr)),
  f("decodeUTF8", Sig::Simple(&[Param::new("arr", Ty::NumArr)], Ty::Str)),
  f("manifestIni", Sig::Simple(&[Param::new("ini", Ty::Object)], Ty::Str)),
  f("manifestPython", Sig::Simple(&[Param::new("v", Ty::Any)], Ty::Str)),
  f("manifestPythonVars", Sig::Simple(&[Param::new("conf", Ty::Any)], Ty::Str)),
  f(
    "manifestJsonEx",
    Sig::Simple(
      &[
        Param::new("value", Ty::Any),
        Param::new("indent", Ty::Str),
        Param::new("newline", Ty::Str),
        Param::new("key_val_sep", Ty::Str),
      ],
      Ty::Str,
    ),
  ),
  f("manifestJson", Sig::Simple(&[Param::new("value", Ty::Any)], Ty::Str)),
  f("manifestJsonMinified", Sig::Complex(&["value"])),
  f("manifestYamlDoc", Sig::Complex(&["value", "indent_array_in_object", "quote_keys"])),
  f(
    "manifestYamlStream",
    Sig::Complex(&["value", "indent_array_in_object", "c_document_end", "quote_keys"]),
  ),
  f("manifestXmlJsonml", Sig::Complex(&["value"])),
  f("manifestTomlEx", Sig::Complex(&["toml", "indent"])),
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
  f("range", Sig::Complex(&["from", "to"])),
  f("repeat", Sig::Complex(&["what", "count"])),
  f("slice", Sig::Complex(&["indexable", "index", "end", "step"])),
  f("join", Sig::Complex(&["sep", "arr"])),
  f("lines", Sig::Simple(&[Param::new("arr", Ty::StrArr)], Ty::Str)),
  f("flattenArrays", Sig::Complex(&["arr"])),
  f("reverse", Sig::Complex(&["arr"])),
  f("sort", Sig::Complex(&["arr", "keyF"])),
  f("uniq", Sig::Complex(&["arr", "keyF"])),
  f("all", Sig::Simple(&[Param::new("arr", Ty::BoolArr)], Ty::Bool)),
  f("any", Sig::Simple(&[Param::new("arr", Ty::BoolArr)], Ty::Bool)),
  f("sum", Sig::Simple(&[Param::new("arr", Ty::NumArr)], Ty::Num)),
  f("avg", Sig::Simple(&[Param::new("arr", Ty::NumArr)], Ty::Num)),
  f("set", Sig::Complex(&["arr", "keyF"])),
  f("setInter", Sig::Complex(&["a", "b", "keyF"])),
  f("setUnion", Sig::Complex(&["a", "b", "keyF"])),
  f("setDiff", Sig::Complex(&["a", "b", "keyF"])),
  f("setMember", Sig::Complex(&["x", "arr", "keyF"])),
  f("base64", Sig::Complex(&["input"])),
  f("base64DecodeBytes", Sig::Complex(&["str"])),
  f("base64Decode", STR_RET_STR),
  f("md5", Sig::Simple(&[Param::new("s", Ty::Str)], Ty::Str)),
  f("xor", X_Y_BOOL_RET_BOOL),
  f("xnor", X_Y_BOOL_RET_BOOL),
  f("mergePatch", Sig::Complex(&["target", "patch"])),
  f("trace", Sig::Complex(&["str", "rest"])),
  // alluded to in the spec but not mentioned on the std lib page
  f("equals", Sig::Complex(&["x", "y"])),
  f("objectHasEx", Sig::Complex(&["obj", "fname", "hidden"])),
];
