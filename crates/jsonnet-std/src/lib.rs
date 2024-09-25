//! The names and parameter names of the std lib fns.

/// A name-content string pair.
#[derive(Debug, Clone, Copy)]
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
#[derive(Debug)]
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
#[derive(Debug)]
pub enum Sig {
  /// A regular signature, expressible in a simple type system. It has parameters and a return type.
  Regular(&'static [Param], Ty),
  /// A special signature with complex type handling.
  Special(&'static [&'static str]),
}

/// A function parameter.
#[derive(Debug)]
pub struct Param {
  /// Its name.
  pub name: &'static str,
  /// Its type.
  pub ty: Ty,
  /// Whether it is required.
  pub required: bool,
}

/// A simple type.
#[derive(Debug)]
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

/// The std fns.
pub const FNS: [Fn; 121] = [
  Fn::new("extVar", &["x"]),
  Fn::named("type", "type_", &["x"]),
  Fn::new("isArray", &["v"]),
  Fn::new("isBoolean", &["v"]),
  Fn::new("isFunction", &["v"]),
  Fn::new("isNumber", &["v"]),
  Fn::new("isObject", &["v"]),
  Fn::new("isString", &["v"]),
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
  Fn::new("exp", &["x"]),
  Fn::new("log", &["x"]),
  Fn::new("exponent", &["x"]),
  Fn::new("mantissa", &["x"]),
  Fn::new("floor", &["x"]),
  Fn::new("ceil", &["x"]),
  Fn::new("sqrt", &["x"]),
  Fn::new("sin", &["x"]),
  Fn::new("cos", &["x"]),
  Fn::new("tan", &["x"]),
  Fn::new("asin", &["x"]),
  Fn::new("acos", &["x"]),
  Fn::new("atan", &["x"]),
  Fn::new("round", &["x"]),
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
  Fn::new("asciiUpper", &["str"]),
  Fn::new("asciiLower", &["str"]),
  Fn::new("stringChars", &["str"]),
  Fn::new("format", &["str", "vals"]),
  Fn::new("escapeStringBash", &["str"]),
  Fn::new("escapeStringDollars", &["str"]),
  Fn::new("escapeStringJson", &["str"]),
  Fn::new("escapeStringPython", &["str"]),
  Fn::new("escapeStringXml", &["str"]),
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
