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
  /// A regular signature, expressible in a simple type system. It has parameters and a return type.
  Regular(&'static [Param], Ty),
  /// A special signature with complex type handling.
  Special(&'static [&'static str]),
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
  Boolean,
  /// A number like `0` or `123` or `-456`.
  Number,
  /// A string like `"foo"` or `"bar"` or `""`.
  String,
}

const V_ANY_RET_BOOL: Sig = Sig::Regular(&[Param::new("v", Ty::Any)], Ty::Boolean);
const X_NUM_RET_NUM: Sig = Sig::Regular(&[Param::new("x", Ty::Number)], Ty::Number);
const X_NUM_RET_BOOL: Sig = Sig::Regular(&[Param::new("x", Ty::Number)], Ty::Boolean);
const STR_RET_STR: Sig = Sig::Regular(&[Param::new("str", Ty::String)], Ty::String);

const fn f(name: &'static str, sig: Sig) -> Fn {
  Fn { name: S::new(name), sig }
}

/// The std fns.
pub const FNS: [Fn; 127] = [
  f("extVar", Sig::Regular(&[Param::new("x", Ty::String)], Ty::String)),
  Fn {
    name: S::named("type", "type_"),
    sig: Sig::Regular(&[Param::new("x", Ty::Any)], Ty::String),
  },
  f("isArray", V_ANY_RET_BOOL),
  f("isBoolean", V_ANY_RET_BOOL),
  f("isFunction", V_ANY_RET_BOOL),
  f("isNumber", V_ANY_RET_BOOL),
  f("isObject", V_ANY_RET_BOOL),
  f("isString", V_ANY_RET_BOOL),
  f("length", Sig::Special(&["x"])),
  f("get", Sig::Special(&["o", "f", "default", "inc_hidden"])),
  f("objectHas", Sig::Special(&["o", "f"])),
  f("objectFields", Sig::Special(&["o"])),
  f("objectValues", Sig::Special(&["o"])),
  f("objectKeysValues", Sig::Special(&["o"])),
  f("objectHasAll", Sig::Special(&["o", "f"])),
  f("objectFieldsAll", Sig::Special(&["o"])),
  f("objectValuesAll", Sig::Special(&["o"])),
  f("objectKeysValuesAll", Sig::Special(&["o"])),
  f("prune", Sig::Special(&["a"])),
  f("mapWithKey", Sig::Special(&["func", "obj"])),
  f("abs", Sig::Special(&["n"])),
  f("sign", Sig::Special(&["n"])),
  f("max", Sig::Special(&["a", "b"])),
  f("min", Sig::Special(&["a", "b"])),
  f("pow", Sig::Special(&["x", "n"])),
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
  Fn { name: S::named("mod", "mod_"), sig: Sig::Special(&["a", "b"]) },
  f("clamp", Sig::Special(&["x", "minVal", "maxVal"])),
  f("assertEqual", Sig::Special(&["a", "b"])),
  f("toString", Sig::Special(&["a"])),
  f("codepoint", Sig::Special(&["str"])),
  f("char", Sig::Special(&["n"])),
  f("substr", Sig::Special(&["str", "from", "len"])),
  f("findSubstr", Sig::Special(&["pat", "str"])),
  f("startsWith", Sig::Special(&["a", "b"])),
  f("endsWith", Sig::Special(&["a", "b"])),
  f("stripChars", Sig::Special(&["str", "chars"])),
  f("lstripChars", Sig::Special(&["str", "chars"])),
  f("rstripChars", Sig::Special(&["str", "chars"])),
  f("split", Sig::Special(&["str", "c"])),
  f("splitLimit", Sig::Special(&["str", "c", "maxsplits"])),
  f("splitLimitR", Sig::Special(&["str", "c", "maxsplits"])),
  f("strReplace", Sig::Special(&["str", "from", "to"])),
  f("isEmpty", Sig::Special(&["str"])),
  f("asciiUpper", STR_RET_STR),
  f("asciiLower", STR_RET_STR),
  f("stringChars", STR_RET_STR),
  f("format", Sig::Special(&["str", "vals"])),
  f("escapeStringBash", STR_RET_STR),
  f("escapeStringDollars", STR_RET_STR),
  f("escapeStringJson", STR_RET_STR),
  f("escapeStringPython", STR_RET_STR),
  f("escapeStringXml", STR_RET_STR),
  f("parseInt", Sig::Special(&["str"])),
  f("parseOctal", Sig::Special(&["str"])),
  f("parseHex", Sig::Special(&["str"])),
  f("parseJson", Sig::Special(&["str"])),
  f("parseYaml", Sig::Special(&["str"])),
  f("encodeUTF8", Sig::Special(&["str"])),
  f("decodeUTF8", Sig::Special(&["arr"])),
  f("manifestIni", Sig::Special(&["ini"])),
  f("manifestPython", Sig::Special(&["v"])),
  f("manifestPythonVars", Sig::Special(&["conf"])),
  f("manifestJsonEx", Sig::Special(&["value", "indent", "newline", "key_val_sep"])),
  f("manifestJson", Sig::Special(&["value"])),
  f("manifestJsonMinified", Sig::Special(&["value"])),
  f("manifestYamlDoc", Sig::Special(&["value", "indent_array_in_object", "quote_keys"])),
  f(
    "manifestYamlStream",
    Sig::Special(&["value", "indent_array_in_object", "c_document_end", "quote_keys"]),
  ),
  f("manifestXmlJsonml", Sig::Special(&["value"])),
  f("manifestTomlEx", Sig::Special(&["toml", "indent"])),
  f("makeArray", Sig::Special(&["sz", "func"])),
  f("member", Sig::Special(&["arr", "x"])),
  f("count", Sig::Special(&["arr", "x"])),
  f("find", Sig::Special(&["value", "arr"])),
  f("map", Sig::Special(&["func", "arr"])),
  f("mapWithIndex", Sig::Special(&["func", "arr"])),
  f("filterMap", Sig::Special(&["filter_func", "map_func", "arr"])),
  f("flatMap", Sig::Special(&["func", "arr"])),
  f("filter", Sig::Special(&["func", "arr"])),
  f("foldl", Sig::Special(&["func", "arr", "init"])),
  f("foldr", Sig::Special(&["func", "arr", "init"])),
  f("range", Sig::Special(&["from", "to"])),
  f("repeat", Sig::Special(&["what", "count"])),
  f("slice", Sig::Special(&["indexable", "index", "end", "step"])),
  f("join", Sig::Special(&["sep", "arr"])),
  f("lines", Sig::Special(&["arr"])),
  f("flattenArrays", Sig::Special(&["arr"])),
  f("reverse", Sig::Special(&["arrs"])),
  f("sort", Sig::Special(&["arr", "keyF"])),
  f("uniq", Sig::Special(&["arr", "keyF"])),
  f("all", Sig::Special(&["arr"])),
  f("any", Sig::Special(&["arr"])),
  f("sum", Sig::Special(&["arr"])),
  f("avg", Sig::Special(&["arr"])),
  f("set", Sig::Special(&["arr", "keyF"])),
  f("setInter", Sig::Special(&["a", "b", "keyF"])),
  f("setUnion", Sig::Special(&["a", "b", "keyF"])),
  f("setDiff", Sig::Special(&["a", "b", "keyF"])),
  f("setMember", Sig::Special(&["x", "arr", "keyF"])),
  f("base64", Sig::Special(&["input"])),
  f("base64DecodeBytes", Sig::Special(&["str"])),
  f("base64Decode", Sig::Special(&["str"])),
  f("md5", Sig::Special(&["s"])),
  f("xor", Sig::Special(&["x", "y"])),
  f("xnor", Sig::Special(&["x", "y"])),
  f("mergePatch", Sig::Special(&["target", "patch"])),
  f("trace", Sig::Special(&["str", "rest"])),
  // alluded to in the spec but not mentioned on the std lib page
  f("cmp", Sig::Special(&["a", "b"])),
  f("equals", Sig::Special(&["a", "b"])),
  f("objectHasEx", Sig::Special(&["o", "f"])),
];
