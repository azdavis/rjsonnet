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

  /// Returns the name. Must be a valid Rust identifier.
  #[must_use]
  pub const fn name(&self) -> &'static str {
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

/// The std fns.
pub const FNS: [(S, &[&str]); 121] = [
  (S::new("extVar"), ["x"].as_slice()),
  (S::named("type", "type_"), &["x"]),
  (S::new("isArray"), &["v"]),
  (S::new("isBoolean"), &["v"]),
  (S::new("isFunction"), &["v"]),
  (S::new("isNumber"), &["v"]),
  (S::new("isObject"), &["v"]),
  (S::new("isString"), &["v"]),
  (S::new("length"), &["x"]),
  (S::new("get"), &["o", "f", "default", "inc_hidden"]),
  (S::new("objectHas"), &["o", "f"]),
  (S::new("objectFields"), &["o"]),
  (S::new("objectValues"), &["o"]),
  (S::new("objectKeysValues"), &["o"]),
  (S::new("objectHasAll"), &["o", "f"]),
  (S::new("objectFieldsAll"), &["o"]),
  (S::new("objectValuesAll"), &["o"]),
  (S::new("objectKeysValuesAll"), &["o"]),
  (S::new("prune"), &["a"]),
  (S::new("mapWithKey"), &["func", "obj"]),
  (S::new("abs"), &["n"]),
  (S::new("sign"), &["n"]),
  (S::new("max"), &["a", "b"]),
  (S::new("min"), &["a", "b"]),
  (S::new("pow"), &["x", "n"]),
  (S::new("exp"), &["x"]),
  (S::new("log"), &["x"]),
  (S::new("exponent"), &["x"]),
  (S::new("mantissa"), &["x"]),
  (S::new("floor"), &["x"]),
  (S::new("ceil"), &["x"]),
  (S::new("sqrt"), &["x"]),
  (S::new("sin"), &["x"]),
  (S::new("cos"), &["x"]),
  (S::new("tan"), &["x"]),
  (S::new("asin"), &["x"]),
  (S::new("acos"), &["x"]),
  (S::new("atan"), &["x"]),
  (S::new("round"), &["x"]),
  (S::named("mod", "mod_"), &["a", "b"]),
  (S::new("clamp"), &["x", "minVal", "maxVal"]),
  (S::new("assertEqual"), &["a", "b"]),
  (S::new("toString"), &["a"]),
  (S::new("codepoint"), &["str"]),
  (S::new("char"), &["n"]),
  (S::new("substr"), &["str", "from", "len"]),
  (S::new("findSubstr"), &["pat", "str"]),
  (S::new("startsWith"), &["a", "b"]),
  (S::new("endsWith"), &["a", "b"]),
  (S::new("stripChars"), &["str", "chars"]),
  (S::new("lstripChars"), &["str", "chars"]),
  (S::new("rstripChars"), &["str", "chars"]),
  (S::new("split"), &["str", "c"]),
  (S::new("splitLimit"), &["str", "c", "maxsplits"]),
  (S::new("splitLimitR"), &["str", "c", "maxsplits"]),
  (S::new("strReplace"), &["str", "from", "to"]),
  (S::new("isEmpty"), &["str"]),
  (S::new("asciiUpper"), &["str"]),
  (S::new("asciiLower"), &["str"]),
  (S::new("stringChars"), &["str"]),
  (S::new("format"), &["str", "vals"]),
  (S::new("escapeStringBash"), &["str"]),
  (S::new("escapeStringDollars"), &["str"]),
  (S::new("escapeStringJson"), &["str"]),
  (S::new("escapeStringPython"), &["str"]),
  (S::new("escapeStringXml"), &["str"]),
  (S::new("parseInt"), &["str"]),
  (S::new("parseOctal"), &["str"]),
  (S::new("parseHex"), &["str"]),
  (S::new("parseJson"), &["str"]),
  (S::new("parseYaml"), &["str"]),
  (S::new("encodeUTF8"), &["str"]),
  (S::new("decodeUTF8"), &["arr"]),
  (S::new("manifestIni"), &["ini"]),
  (S::new("manifestPython"), &["v"]),
  (S::new("manifestPythonVars"), &["conf"]),
  (S::new("manifestJsonEx"), &["value", "indent", "newline", "key_val_sep"]),
  (S::new("manifestJsonMinified"), &["value"]),
  (S::new("manifestYamlDoc"), &["value", "indent_array_in_object", "quote_keys"]),
  (
    S::new("manifestYamlStream"),
    &["value", "indent_array_in_object", "c_document_end", "quote_keys"],
  ),
  (S::new("manifestXmlJsonml"), &["value"]),
  (S::new("manifestTomlEx"), &["toml", "indent"]),
  (S::new("makeArray"), &["sz", "func"]),
  (S::new("member"), &["arr", "x"]),
  (S::new("count"), &["arr", "x"]),
  (S::new("find"), &["value", "arr"]),
  (S::new("map"), &["func", "arr"]),
  (S::new("mapWithIndex"), &["func", "arr"]),
  (S::new("filterMap"), &["filter_func", "map_func", "arr"]),
  (S::new("flatMap"), &["func", "arr"]),
  (S::new("filter"), &["func", "arr"]),
  (S::new("foldl"), &["func", "arr", "init"]),
  (S::new("foldr"), &["func", "arr", "init"]),
  (S::new("range"), &["from", "to"]),
  (S::new("repeat"), &["what", "count"]),
  (S::new("slice"), &["indexable", "index", "end", "step"]),
  (S::new("join"), &["sep", "arr"]),
  (S::new("lines"), &["arr"]),
  (S::new("flattenArrays"), &["arr"]),
  (S::new("reverse"), &["arrs"]),
  (S::new("sort"), &["arr", "keyF"]),
  (S::new("uniq"), &["arr", "keyF"]),
  (S::new("all"), &["arr"]),
  (S::new("any"), &["arr"]),
  (S::new("sum"), &["arr"]),
  (S::new("set"), &["arr", "keyF"]),
  (S::new("setInter"), &["a", "b", "keyF"]),
  (S::new("setUnion"), &["a", "b", "keyF"]),
  (S::new("setDiff"), &["a", "b", "keyF"]),
  (S::new("setMember"), &["x", "arr", "keyF"]),
  (S::new("base64"), &["input"]),
  (S::new("base64DecodeBytes"), &["str"]),
  (S::new("base64Decode"), &["str"]),
  (S::new("md5"), &["s"]),
  (S::new("xor"), &["x", "y"]),
  (S::new("xnor"), &["x", "y"]),
  (S::new("mergePatch"), &["target", "patch"]),
  (S::new("trace"), &["str", "rest"]),
  // alluded to in the spec but not mentioned on the std lib page
  (S::new("cmp"), &["a", "b"]),
  (S::new("equals"), &["a", "b"]),
  (S::new("objectHasEx"), &["o", "f"]),
];
