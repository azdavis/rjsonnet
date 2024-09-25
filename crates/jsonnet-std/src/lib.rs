//! The names and parameter names of the std lib fns.

/// A name-content string pair.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct S {
  /// The name. If None, the content is the name.
  name: Option<&'static str>,
  /// The content.
  content: &'static str,
}

impl S {
  /// Make a new k-v pair (actually the param order is v first then k).
  #[must_use]
  pub const fn named(content: &'static str, name: &'static str) -> S {
    // NOTE: would like to check content != name, but not available in const context at time of
    // writing.
    S { name: Some(name), content }
  }

  /// Make a new one whose name (k) is the content (v).
  #[must_use]
  pub const fn new(content: &'static str) -> S {
    S { name: None, content }
  }

  /// Returns the identifier. Must be a valid Rust identifier.
  #[must_use]
  pub const fn ident(&self) -> &'static str {
    match self.name {
      Some(x) => x,
      None => self.content,
    }
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

impl Fn {
  const fn new(name: &'static str, params: &'static [&'static str]) -> Self {
    Self { name: S::new(name), sig: Sig::Special(params) }
  }

  const fn named(
    content: &'static str,
    name: &'static str,
    params: &'static [&'static str],
  ) -> Self {
    Self { name: S::named(content, name), sig: Sig::Special(params) }
  }
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

/// The std fns.
pub const FNS: [Fn; 127] = [
  Fn { name: S::new("extVar"), sig: Sig::Regular(&[Param::new("x", Ty::String)], Ty::String) },
  Fn {
    name: S::named("type", "type_"),
    sig: Sig::Regular(&[Param::new("x", Ty::Any)], Ty::String),
  },
  Fn { name: S::new("isArray"), sig: V_ANY_RET_BOOL },
  Fn { name: S::new("isBoolean"), sig: V_ANY_RET_BOOL },
  Fn { name: S::new("isFunction"), sig: V_ANY_RET_BOOL },
  Fn { name: S::new("isNumber"), sig: V_ANY_RET_BOOL },
  Fn { name: S::new("isObject"), sig: V_ANY_RET_BOOL },
  Fn { name: S::new("isString"), sig: V_ANY_RET_BOOL },
  Fn::new("length", &["x"]),
  Fn::new("get", &["o", "f", "default", "inc_hidden"]),
  Fn::new("objectHas", &["o", "f"]),
  Fn::new("objectFields", &["o"]),
  Fn::new("objectValues", &["o"]),
  Fn::new("objectKeysValues", &["o"]),
  Fn::new("objectHasAll", &["o", "f"]),
  Fn::new("objectFieldsAll", &["o"]),
  Fn::new("objectValuesAll", &["o"]),
  Fn::new("objectKeysValuesAll", &["o"]),
  Fn::new("prune", &["a"]),
  Fn::new("mapWithKey", &["func", "obj"]),
  Fn::new("abs", &["n"]),
  Fn::new("sign", &["n"]),
  Fn::new("max", &["a", "b"]),
  Fn::new("min", &["a", "b"]),
  Fn::new("pow", &["x", "n"]),
  Fn { name: S::new("exp"), sig: X_NUM_RET_NUM },
  Fn { name: S::new("log"), sig: X_NUM_RET_NUM },
  Fn { name: S::new("exponent"), sig: X_NUM_RET_NUM },
  Fn { name: S::new("mantissa"), sig: X_NUM_RET_NUM },
  Fn { name: S::new("floor"), sig: X_NUM_RET_NUM },
  Fn { name: S::new("ceil"), sig: X_NUM_RET_NUM },
  Fn { name: S::new("sqrt"), sig: X_NUM_RET_NUM },
  Fn { name: S::new("sin"), sig: X_NUM_RET_NUM },
  Fn { name: S::new("cos"), sig: X_NUM_RET_NUM },
  Fn { name: S::new("tan"), sig: X_NUM_RET_NUM },
  Fn { name: S::new("asin"), sig: X_NUM_RET_NUM },
  Fn { name: S::new("acos"), sig: X_NUM_RET_NUM },
  Fn { name: S::new("atan"), sig: X_NUM_RET_NUM },
  Fn { name: S::new("round"), sig: X_NUM_RET_NUM },
  Fn { name: S::new("isEven"), sig: X_NUM_RET_BOOL },
  Fn { name: S::new("isOdd"), sig: X_NUM_RET_BOOL },
  Fn { name: S::new("isInteger"), sig: X_NUM_RET_BOOL },
  Fn { name: S::new("isDecimal"), sig: X_NUM_RET_BOOL },
  Fn::named("mod", "mod_", &["a", "b"]),
  Fn::new("clamp", &["x", "minVal", "maxVal"]),
  Fn::new("assertEqual", &["a", "b"]),
  Fn::new("toString", &["a"]),
  Fn::new("codepoint", &["str"]),
  Fn::new("char", &["n"]),
  Fn::new("substr", &["str", "from", "len"]),
  Fn::new("findSubstr", &["pat", "str"]),
  Fn::new("startsWith", &["a", "b"]),
  Fn::new("endsWith", &["a", "b"]),
  Fn::new("stripChars", &["str", "chars"]),
  Fn::new("lstripChars", &["str", "chars"]),
  Fn::new("rstripChars", &["str", "chars"]),
  Fn::new("split", &["str", "c"]),
  Fn::new("splitLimit", &["str", "c", "maxsplits"]),
  Fn::new("splitLimitR", &["str", "c", "maxsplits"]),
  Fn::new("strReplace", &["str", "from", "to"]),
  Fn::new("isEmpty", &["str"]),
  Fn { name: S::new("asciiUpper"), sig: STR_RET_STR },
  Fn { name: S::new("asciiLower"), sig: STR_RET_STR },
  Fn { name: S::new("stringChars"), sig: STR_RET_STR },
  Fn::new("format", &["str", "vals"]),
  Fn { name: S::new("escapeStringBash"), sig: STR_RET_STR },
  Fn { name: S::new("escapeStringDollars"), sig: STR_RET_STR },
  Fn { name: S::new("escapeStringJson"), sig: STR_RET_STR },
  Fn { name: S::new("escapeStringPython"), sig: STR_RET_STR },
  Fn { name: S::new("escapeStringXml"), sig: STR_RET_STR },
  Fn::new("parseInt", &["str"]),
  Fn::new("parseOctal", &["str"]),
  Fn::new("parseHex", &["str"]),
  Fn::new("parseJson", &["str"]),
  Fn::new("parseYaml", &["str"]),
  Fn::new("encodeUTF8", &["str"]),
  Fn::new("decodeUTF8", &["arr"]),
  Fn::new("manifestIni", &["ini"]),
  Fn::new("manifestPython", &["v"]),
  Fn::new("manifestPythonVars", &["conf"]),
  Fn::new("manifestJsonEx", &["value", "indent", "newline", "key_val_sep"]),
  Fn::new("manifestJson", &["value"]),
  Fn::new("manifestJsonMinified", &["value"]),
  Fn::new("manifestYamlDoc", &["value", "indent_array_in_object", "quote_keys"]),
  Fn::new(
    "manifestYamlStream",
    &["value", "indent_array_in_object", "c_document_end", "quote_keys"],
  ),
  Fn::new("manifestXmlJsonml", &["value"]),
  Fn::new("manifestTomlEx", &["toml", "indent"]),
  Fn::new("makeArray", &["sz", "func"]),
  Fn::new("member", &["arr", "x"]),
  Fn::new("count", &["arr", "x"]),
  Fn::new("find", &["value", "arr"]),
  Fn::new("map", &["func", "arr"]),
  Fn::new("mapWithIndex", &["func", "arr"]),
  Fn::new("filterMap", &["filter_func", "map_func", "arr"]),
  Fn::new("flatMap", &["func", "arr"]),
  Fn::new("filter", &["func", "arr"]),
  Fn::new("foldl", &["func", "arr", "init"]),
  Fn::new("foldr", &["func", "arr", "init"]),
  Fn::new("range", &["from", "to"]),
  Fn::new("repeat", &["what", "count"]),
  Fn::new("slice", &["indexable", "index", "end", "step"]),
  Fn::new("join", &["sep", "arr"]),
  Fn::new("lines", &["arr"]),
  Fn::new("flattenArrays", &["arr"]),
  Fn::new("reverse", &["arrs"]),
  Fn::new("sort", &["arr", "keyF"]),
  Fn::new("uniq", &["arr", "keyF"]),
  Fn::new("all", &["arr"]),
  Fn::new("any", &["arr"]),
  Fn::new("sum", &["arr"]),
  Fn::new("avg", &["arr"]),
  Fn::new("set", &["arr", "keyF"]),
  Fn::new("setInter", &["a", "b", "keyF"]),
  Fn::new("setUnion", &["a", "b", "keyF"]),
  Fn::new("setDiff", &["a", "b", "keyF"]),
  Fn::new("setMember", &["x", "arr", "keyF"]),
  Fn::new("base64", &["input"]),
  Fn::new("base64DecodeBytes", &["str"]),
  Fn::new("base64Decode", &["str"]),
  Fn::new("md5", &["s"]),
  Fn::new("xor", &["x", "y"]),
  Fn::new("xnor", &["x", "y"]),
  Fn::new("mergePatch", &["target", "patch"]),
  Fn::new("trace", &["str", "rest"]),
  // alluded to in the spec but not mentioned on the std lib page
  Fn::new("cmp", &["a", "b"]),
  Fn::new("equals", &["a", "b"]),
  Fn::new("objectHasEx", &["o", "f"]),
];
