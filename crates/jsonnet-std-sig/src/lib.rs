//! Info about the standard library functions.
//!
//! Based on [the original std lib docs](https://jsonnet.org/ref/stdlib.html).

#![allow(clippy::needless_raw_string_hashes)]

#[cfg(test)]
mod tests;

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
  /// Whether this is implemented.
  pub implemented: bool,
  /// The signature.
  pub sig: Sig,
  /// Whether the function returns a value for all well-typed inputs.
  pub total: bool,
  /// Since what Jsonnet version this is available. If `Some(n)`, this is available since Jsonnet
  /// version 0.n.0. If `None`, unknown.
  pub available_since: Option<u8>,
  /// The documentation.
  pub doc: &'static str,
  /// Some examples to show in the doc.
  ///
  /// A list of Jsonnet expression that should all evaluate to `true`.
  pub examples: &'static [&'static str],
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
  /// The default value for this param. If `None`, the param is required.
  pub default: Option<&'static str>,
}

impl Param {
  /// Returns whether this parameter is required.
  #[must_use]
  pub fn is_required(&self) -> bool {
    self.default.is_none()
  }
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
  /// A string that is interned.
  StrInterned,
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
  Param { name, ty, default: None }
}

const fn opt(name: &'static str, ty: Ty, default: &'static str) -> Param {
  Param { name, ty, default: Some(default) }
}

const fn sig(params: &'static [Param], ret: Ty) -> Sig {
  Sig { params, ret }
}

const KEY_F: Param = opt("keyF", Ty::Hof1, "function(x) x");
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
const ARR_KEY_F: Sig = sig(&[req("arr", Ty::ArrAny), KEY_F], Ty::ArrAny);
const BINARY_SET_FN: Sig = sig(&[req("a", Ty::ArrAny), req("b", Ty::ArrAny), KEY_F], Ty::ArrAny);

/// The std fns.

pub const FNS: [Fn; 126] = [
  Fn {
    name: S::new("extVar"),
    implemented: false,
    sig: sig(&[req("x", Ty::Str)], Ty::Str),
    total: true,
    available_since: Some(10),
    doc: indoc! {"
      If an external variable with the given name was defined, return its value. Otherwise, raise
      an error.
    "},
    examples: &[],
  },
  Fn {
    name: S::named("type", "type_"),
    implemented: true,
    sig: sig(&[req("x", Ty::Any)], Ty::StrInterned),
    total: true,
    available_since: Some(10),
    doc: indoc! {r#"
      Returns a string that indicates the type of the value. The possible return values are:

      - `"array"`
      - `"boolean"`
      - `"function"`
      - `"null"`
      - `"number"`
      - `"object"`
      - `"string"`
    "#},
    examples: &[
      r#" std.type([1]) == "array" "#,
      r#" std.type(null) == "null" "#,
      r#" std.type({}) == "object" "#,
      r#" std.type(3) == "number" "#,
    ],
  },
  Fn {
    name: S::new("isArray"),
    implemented: true,
    sig: V_ANY_RET_BOOL,
    total: true,
    available_since: None,
    doc: indoc! {"
      Returns whether the argument is an array.
    "},
    examples: &["std.isArray([1, 2])", "std.isArray([])", "!std.isArray(null)", "!std.isArray(4)"],
  },
  Fn {
    name: S::new("isBoolean"),
    implemented: true,
    sig: V_ANY_RET_BOOL,
    total: true,
    available_since: None,
    doc: indoc! {"
      Returns whether the argument is a boolean.
    "},
    examples: &[
      "std.isBoolean(true)",
      "std.isBoolean(false)",
      "!std.isBoolean(null)",
      "!std.isBoolean(4)",
    ],
  },
  Fn {
    name: S::new("isFunction"),
    implemented: true,
    sig: V_ANY_RET_BOOL,
    total: true,
    available_since: None,
    doc: indoc! {"
      Returns whether the argument is a function.
    "},
    examples: &[
      "std.isFunction(function(x) x + 1)",
      "std.isFunction(std.mod)",
      "!std.isFunction(null)",
      "!std.isFunction(4)",
    ],
  },
  Fn {
    name: S::new("isNumber"),
    implemented: true,
    sig: V_ANY_RET_BOOL,
    total: true,
    available_since: None,
    doc: indoc! {"
      Returns whether the argument is a number.
    "},
    examples: &[
      "std.isNumber(3)",
      "std.isNumber(-123.345)",
      "!std.isNumber(null)",
      "!std.isNumber([])",
    ],
  },
  Fn {
    name: S::new("isObject"),
    implemented: true,
    sig: V_ANY_RET_BOOL,
    total: true,
    available_since: None,
    doc: indoc! {"
      Returns whether the argument is an object.
    "},
    examples: &[
      "std.isObject({})",
      "std.isObject({ a: 1 } + { b: 2 })",
      "!std.isObject(null)",
      "!std.isObject([])",
    ],
  },
  Fn {
    name: S::new("isString"),
    implemented: true,
    sig: V_ANY_RET_BOOL,
    total: true,
    available_since: None,
    doc: indoc! {"
      Returns whether the argument is a string.
    "},
    examples: &[
      r#" std.isString("hi") "#,
      r#" std.isString("") "#,
      r#" !std.isString(null) "#,
      r#" !std.isString({}) "#,
    ],
  },
  Fn {
    name: S::new("length"),
    implemented: true,
    sig: sig(&[req("x", Ty::Any)], Ty::Uint),
    total: false,
    available_since: Some(10),
    doc: indoc! {r#"
      Depending on the type of the value given, this functions returns the number of
      _something_ in that argument value. The table below describes the _something_:

      | Type     | Something  |
      | -------- | ---------- |
      | array    | elements   |
      | string   | codepoints |
      | function | parameters |
      | object   | fields     |

      Raises an error if given `null`, `true`, `false`, or a number.
    "#},
    examples: &[
      r#" std.length("hi") == 2 "#,
      r#" std.length("") == 0 "#,
      r#" std.length("あ") == 1 "#,
      r#" std.length([]) == 0 "#,
      r#" std.length([3, 4]) == 2 "#,
      r#" std.length(function(x) x + 1) == 1 "#,
      r#" std.length(function() 4) == 0 "#,
      r#" std.length(function(x=1) x + 2) == 0 "#,
      r#" std.length({}) == 0 "#,
      r#" std.length({ a: 3, b: 5 }) == 2 "#,
      r#" std.length({ x:: 9, y::: 7 }) == 2 "#,
    ],
  },
  Fn {
    name: S::new("get"),
    implemented: false,
    sig: sig(
      &[
        req("o", Ty::Obj),
        req("f", Ty::StrInterned),
        opt("default", Ty::Any, "null"),
        opt("inc_hidden", Ty::Bool, "true"),
      ],
      Ty::Any,
    ),
    total: true,
    available_since: Some(18),
    doc: indoc! {"
      Returns the object `o`'s field `f` if it exists or `default` value otherwise. `inc_hidden`
      controls whether to include hidden fields.
    "},
    examples: &[
      r#" std.get({hi: 4}, "hi", 3) == 4 "#,
      r#" std.get({}, "hi", 3) == 3 "#,
      r#" std.get({hi:: 5}, "hi", 3) == 5 "#,
      r#" std.get({hi:: 5}, "hi", 3, false) == 3 "#,
    ],
  },
  Fn {
    name: S::new("objectHas"),
    implemented: false,
    sig: OBJ_HAS,
    total: true,
    available_since: Some(10),
    doc: indoc! {"
      Returns whether the given object `o` has the field `f`.

      Returns `false` if the field is hidden.
    "},
    examples: &[
      r#" !std.objectHas({}, "hi") "#,
      r#" std.objectHas({hi: 3}, "hi") "#,
      r#" !std.objectHas({hi:: 3}, "hi") "#,
    ],
  },
  Fn {
    name: S::new("objectFields"),
    implemented: false,
    sig: OBJ_FIELDS,
    total: true,
    available_since: Some(10),
    doc: indoc! {"
      Returns an array of strings, each element being a field from the given object.

      **Does not** include hidden fields.
    "},
    examples: &[
      r#" std.objectFields({}) == [] "#,
      r#" std.objectFields({a: 1, b: 2}) == ["a", "b"] "#,
      r#" std.objectFields({a:: 1, b: 2}) == ["b"] "#,
    ],
  },
  Fn {
    name: S::new("objectValues"),
    implemented: false,
    sig: OBJ_VALUES,
    total: true,
    available_since: Some(17),
    doc: indoc! {"
      Returns an array of the values in the given object.

      **Does not** include hidden fields.
    "},
    examples: &[
      r#" std.objectValues({}) == [] "#,
      r#" std.objectValues({a: 1, b: 2}) == [1, 2] "#,
      r#" std.objectValues({a:: 1, b: 2}) == [2] "#,
    ],
  },
  Fn {
    name: S::new("objectKeysValues"),
    implemented: false,
    sig: OBJ_KEYS_VALUES,
    total: true,
    available_since: Some(20),
    doc: indoc! {"
      Returns an array of objects from the given object, each object having two fields:
      key (string) and value (object).

      **Does not** include hidden fields.
    "},
    examples: &[
      r#" std.objectKeysValues({}) == [] "#,
      r#" std.objectKeysValues({a: 1, b: 2}) == [{key: "a", value: 1 }, { key: "b", value: 2 }] "#,
      r#" std.objectKeysValues({a:: 1, b: 2}) == [{ key: "b", value: 2 }] "#,
    ],
  },
  Fn {
    name: S::new("objectHasAll"),
    implemented: false,
    sig: OBJ_HAS,
    total: true,
    available_since: Some(10),
    doc: indoc! {"
      Like `std.objectHas` but also includes hidden fields.
    "},
    examples: &[
      r#" !std.objectHasAll({}, "hi") "#,
      r#" std.objectHasAll({hi: 3}, "hi") "#,
      r#" std.objectHasAll({hi:: 3}, "hi") "#,
    ],
  },
  Fn {
    name: S::new("objectFieldsAll"),
    implemented: false,
    sig: OBJ_FIELDS,
    total: true,
    available_since: Some(10),
    doc: indoc! {"
      Like `std.objectFields` but also includes hidden fields.
    "},
    examples: &[
      r#" std.objectFieldsAll({}) == [] "#,
      r#" std.objectFieldsAll({a: 1, b: 2}) == ["a", "b"] "#,
      r#" std.objectFieldsAll({a:: 1, b: 2}) == ["a", "b"] "#,
    ],
  },
  Fn {
    name: S::new("objectValuesAll"),
    implemented: false,
    sig: OBJ_VALUES,
    total: true,
    available_since: Some(10),
    doc: indoc! {"
      Like `std.objectValues` but also includes hidden fields.
    "},
    examples: &[
      r#" std.objectValuesAll({}) == [] "#,
      r#" std.objectValuesAll({a: 1, b: 2}) == [1, 2] "#,
      r#" std.objectValuesAll({a:: 1, b: 2}) == [1, 2] "#,
    ],
  },
  Fn {
    name: S::new("objectKeysValuesAll"),
    implemented: false,
    sig: OBJ_KEYS_VALUES,
    total: true,
    available_since: Some(20),
    doc: indoc! {"
      Like `std.objectKeysValues` but also includes hidden fields.
    "},
    examples: &[
      r#" std.objectKeysValuesAll({}) == [] "#,
      r#" std.objectKeysValuesAll({a: 1, b: 2}) == [{key: "a", value: 1 }, { key: "b", value: 2 }] "#,
      r#" std.objectKeysValuesAll({a:: 1, b: 2}) == [{key: "a", value: 1 }, { key: "b", value: 2 }] "#,
    ],
  },
  Fn {
    name: S::new("prune"),
    implemented: false,
    sig: sig(&[req("a", Ty::Any)], Ty::Any),
    total: true,
    available_since: Some(10),
    doc: indoc! {r#"
      Recursively remove all "empty" members of a. "Empty" is defined as

      - zero length arrays
      - zero length objects
      - `null` values

      The argument may have any type.
    "#},
    examples: &[
      r#" std.prune([1, [], 2, {}, 3, null]) == [1, 2, 3] "#,
      r#" std.prune({a: 3}) == {a: 3} "#,
      r#" std.prune({w: 0, x: "", y: [], z: null}) == {w: 0, x: ""} "#,
      r#" std.prune(null) == null "#,
    ],
  },
  Fn {
    name: S::new("mapWithKey"),
    implemented: false,
    sig: sig(&[req("func", Ty::Hof2), req("obj", Ty::Obj)], Ty::Obj),
    total: true,
    available_since: Some(10),
    doc: indoc! {"
      Applies the given `func` to all fields of the given `obj`, also passing the field name.

      `func` is expected to take the field name as the first parameter and the field value as the
      second.
    "},
    examples: &[
      indoc! {r#"
        std.mapWithKey(function(a, b) a + b, {a: 1, b: 2}) == {
          a: "a1",
          b: "b2",
        }
      "#},
      r#" std.mapWithKey(function(a, b) a + b, {a:: 1, b: 2}) == {b: "b2"} "#,
    ],
  },
  Fn {
    name: S::new("abs"),
    implemented: true,
    sig: N_NUM_RET_NUM,
    total: true,
    available_since: None,
    doc: indoc! {"
      Returns the absolute value of the number.
    "},
    examples: &["std.abs(3) == 3", "std.abs(-1.2) == 1.2", "std.abs(0) == 0"],
  },
  Fn {
    name: S::new("sign"),
    implemented: true,
    sig: N_NUM_RET_NUM,
    total: true,
    available_since: None,
    doc: indoc! {"
      Returns `-1`, `0`, or `1` if the number is negative, zero, or positive respectively.
    "},
    examples: &["std.sign(3) == 1", "std.sign(-1.2) == -1", "std.sign(0) == 0"],
  },
  Fn {
    name: S::new("max"),
    implemented: true,
    sig: sig(&[req("a", Ty::Num), req("b", Ty::Num)], Ty::Num),
    total: true,
    available_since: None,
    doc: indoc! {"
      Returns the maximum of the two arguments.
    "},
    examples: &["std.max(3, 2) == 3", "std.max(4, 4) == 4", "std.max(-5, 1) == 1"],
  },
  Fn {
    name: S::new("min"),
    implemented: true,
    sig: sig(&[req("a", Ty::Num), req("b", Ty::Num)], Ty::Num),
    total: true,
    available_since: None,
    doc: indoc! {"
      Returns the minimum of the two arguments.
    "},
    examples: &["std.min(3, 2) == 2", "std.min(4, 4) == 4", "std.min(-5, 1) == -5"],
  },
  Fn {
    name: S::new("pow"),
    implemented: true,
    sig: sig(&[req("x", Ty::Num), req("n", Ty::Num)], Ty::Num),
    total: true,
    available_since: None,
    doc: indoc! {"
      Returns $x^n$, i.e. $x$ to the $n$ power.
    "},
    examples: &[
      "std.pow(2, 3) == 8",
      "std.pow(3, 2) == 9",
      "std.pow(1, 99) == 1",
      "std.pow(0, 2) == 0",
      "std.pow(99, 0) == 1",
    ],
  },
  Fn {
    name: S::new("exp"),
    implemented: true,
    sig: X_NUM_RET_NUM,
    total: true,
    available_since: None,
    doc: indoc! {r"
      Returns $e^x$, i.e. $e$ to the $x$ power, where
      [$e \approx 2.71828$](<https://en.wikipedia.org/wiki/E_(mathematical_constant)>).
    "},
    examples: &[
      "std.exp(0) == 1",
      "std.exp(1) == 2.718281828459045",
      "std.exp(2) == 7.38905609893065",
    ],
  },
  Fn {
    name: S::new("log"),
    implemented: true,
    sig: X_NUM_RET_NUM,
    total: true,
    available_since: None,
    doc: indoc! {r"
      Returns the natural logarithm of $x$,
      i.e. the solution $y$ in $e^y = x$, where
      [$e \approx 2.71828$](<https://en.wikipedia.org/wiki/E_(mathematical_constant)>).
    "},
    examples: &[
      "std.log(1) == 0",
      "std.log(123) == 4.812184355372417",
      "std.log(345) == 5.84354441703136",
    ],
  },
  Fn {
    name: S::new("exponent"),
    implemented: false,
    sig: X_NUM_RET_NUM,
    total: true,
    available_since: None,
    doc: indoc! {r"
      Returns the integer exponent of the IEEE754 64-bit floating point number `x`.

      This is the integer $b$ in the solution of $x = a \times 2^b$.
    "},
    examples: &[
      "std.exponent(0) == 0",
      "std.exponent(4) == 3",
      "std.exponent(123456789) == 27",
      "std.exponent(-789456) == 20",
    ],
  },
  Fn {
    name: S::new("mantissa"),
    implemented: false,
    sig: X_NUM_RET_NUM,
    total: true,
    available_since: None,
    doc: indoc! {"
      Returns the significand, also called the mantissa, of the IEEE754 64-bit floating point
      number `x`.

      This is the number $a$ in the solution of $x = a \times 2^b$ where $b$ is an integer.
    "},
    examples: &[
      "std.mantissa(0) == 0",
      "std.mantissa(4) == 0.5",
      "std.mantissa(123456789) == 0.9198247566819191",
      "std.mantissa(-789456) == -0.7528839111328125",
    ],
  },
  Fn {
    name: S::new("floor"),
    implemented: true,
    sig: X_NUM_RET_NUM,
    total: true,
    available_since: None,
    doc: indoc! {"
      Returns the smallest integer greater than or equal to the argument.
    "},
    examples: &[
      "std.floor(1) == 1",
      "std.floor(1.99) == 1",
      "std.floor(2.01) == 2",
      "std.floor(-1) == -1",
      "std.floor(-1.01) == -2",
      "std.floor(-1.99) == -2",
      "std.floor(-2.01) == -3",
    ],
  },
  Fn {
    name: S::new("ceil"),
    implemented: true,
    sig: X_NUM_RET_NUM,
    total: true,
    available_since: None,
    doc: indoc! {"
      Returns the greatest integer smaller than or equal to the argument.
    "},
    examples: &[
      "std.ceil(1) == 1",
      "std.ceil(1.99) == 2",
      "std.ceil(2.01) == 3",
      "std.ceil(-1) == -1",
      "std.ceil(-1.01) == -1",
      "std.ceil(-1.99) == -1",
      "std.ceil(-2.01) == -2",
    ],
  },
  Fn {
    name: S::new("sqrt"),
    implemented: true,
    sig: X_NUM_RET_NUM,
    total: true,
    available_since: None,
    doc: indoc! {"
      Returns the square root of the argument.
    "},
    examples: &["std.sqrt(9) == 3", "std.sqrt(4) == 2", "std.sqrt(1) == 1"],
  },
  Fn {
    name: S::new("sin"),
    implemented: true,
    sig: X_NUM_RET_NUM,
    total: true,
    available_since: None,
    doc: indoc! {"
      Returns the sine of the argument.
    "},
    examples: &["std.sin(0.5) == 0.479425538604203"],
  },
  Fn {
    name: S::new("cos"),
    implemented: true,
    sig: X_NUM_RET_NUM,
    total: true,
    available_since: None,
    doc: indoc! {"
      Returns the cosine of its argument.
    "},
    examples: &["std.cos(0.5) == 0.8775825618903728"],
  },
  Fn {
    name: S::new("tan"),
    implemented: true,
    sig: X_NUM_RET_NUM,
    total: true,
    available_since: None,
    doc: indoc! {"
      Returns the tangent of its argument.
    "},
    examples: &["std.tan(0.5) == 0.5463024898437905"],
  },
  Fn {
    name: S::new("asin"),
    implemented: true,
    sig: X_NUM_RET_NUM,
    total: true,
    available_since: None,
    doc: indoc! {"
      Returns the arcsine of its argument.
    "},
    examples: &["std.asin(0.5) == 0.5235987755982988"],
  },
  Fn {
    name: S::new("acos"),
    implemented: true,
    sig: X_NUM_RET_NUM,
    total: true,
    available_since: None,
    doc: indoc! {"
      Returns the arccosine of its argument.
      ```
    "},
    examples: &["std.acos(0.5) == 1.0471975511965976"],
  },
  Fn {
    name: S::new("atan"),
    implemented: true,
    sig: X_NUM_RET_NUM,
    total: true,
    available_since: None,
    doc: indoc! {"
      Returns the arctangent of its argument.
    "},
    examples: &["std.atan(0.5) == 0.46364760900080615"],
  },
  Fn {
    name: S::new("round"),
    implemented: true,
    sig: X_NUM_RET_NUM,
    total: true,
    available_since: None,
    doc: indoc! {"
      Returns the argument rounded to the nearest integer.
    "},
    examples: &[
      "std.round(1) == 1",
      "std.round(1.1) == 1",
      "std.round(1.5) == 2",
      "std.round(1.9) == 2",
      "std.round(-1) == -1",
      "std.round(-1.1) == -1",
      "std.round(-1.5) == -2",
      "std.round(-1.9) == -2",
    ],
  },
  Fn {
    name: S::new("isEven"),
    implemented: true,
    sig: X_NUM_RET_BOOL,
    total: true,
    available_since: None,
    doc: indoc! {"
      Returns whether the argument is an even number.

      Raises if the argument is not a number.
    "},
    examples: &[
      "std.isEven(2)",
      "std.isEven(0)",
      "std.isEven(-2)",
      "!std.isEven(1)",
      "!std.isEven(9)",
      "!std.isEven(-5)",
      "!std.isEven(4.4)",
      "!std.isEven(5.5)",
    ],
  },
  Fn {
    name: S::new("isOdd"),
    implemented: true,
    sig: X_NUM_RET_BOOL,
    total: true,
    available_since: None,
    doc: indoc! {"
      Returns whether the argument is an odd number.

      Raises if the argument is not a number.
    "},
    examples: &[
      "std.isOdd(1)",
      "std.isOdd(9)",
      "std.isOdd(-5)",
      "!std.isOdd(2)",
      "!std.isOdd(0)",
      "!std.isOdd(-2)",
      "!std.isOdd(4.4)",
      "!std.isOdd(5.5)",
    ],
  },
  Fn {
    name: S::new("isInteger"),
    implemented: true,
    sig: X_NUM_RET_BOOL,
    total: true,
    available_since: None,
    doc: indoc! {"
      Returns whether the argument is an integer number.

      Raises if the argument is not a number.
    "},
    examples: &[
      "std.isInteger(1)",
      "std.isInteger(0)",
      "std.isInteger(-5)",
      "!std.isInteger(0.1)",
      "!std.isInteger(4.4)",
      "!std.isInteger(2.0001)",
    ],
  },
  Fn {
    name: S::new("isDecimal"),
    implemented: true,
    sig: X_NUM_RET_BOOL,
    total: true,
    available_since: None,
    doc: indoc! {"
      Returns whether the argument is a decimal number, i.e. one with non-zero digits
      after the decimal point.

      Raises if the argument is not a number.
    "},
    examples: &[
      "std.isDecimal(0.1)",
      "std.isDecimal(4.4)",
      "std.isDecimal(2.0001)",
      "!std.isDecimal(1)",
      "!std.isDecimal(0)",
      "!std.isDecimal(-5)",
    ],
  },
  Fn {
    name: S::named("mod", "mod_"),
    implemented: false,
    sig: sig(&[req("a", Ty::NumOrStr), req("b", Ty::Any)], Ty::Any),
    total: false,
    available_since: None,
    doc: indoc! {"
      This is what the `%` operator is desugared to. It performs modulo arithmetic if the left
      hand side is a number, or if the left hand side is a string, it does Python-style string
      formatting with `std.format`.
    "},
    examples: &[],
  },
  Fn {
    name: S::new("clamp"),
    implemented: true,
    sig: sig(&[req("x", Ty::Num), req("minVal", Ty::Num), req("maxVal", Ty::Num)], Ty::Num),
    total: true,
    available_since: Some(15),
    doc: indoc! {"
      Clamps a value to fit within the range `[minVal, maxVal]`.

      Equivalent to `std.max(minVal, std.min(x, maxVal))`.
    "},
    examples: &["std.clamp(-3, 0, 5) == 0", "std.clamp(4, 0, 5) == 4", "std.clamp(7, 0, 5) == 5"],
  },
  Fn {
    name: S::new("assertEqual"),
    implemented: false,
    sig: sig(&[req("a", Ty::Any), req("b", Ty::Any)], Ty::True),
    total: true,
    available_since: Some(10),
    doc: indoc! {"
      Ensures that `a == b` holds.

      Returns `true` if so, else throws an error message.
    "},
    examples: &["std.assertEqual(2 + 2, 4)"],
  },
  Fn {
    name: S::new("toString"),
    implemented: false,
    sig: sig(&[req("a", Ty::Any)], Ty::Str),
    total: true,
    available_since: Some(10),
    doc: indoc! {"
      Converts the given argument to a string.
    "},
    examples: &[
      r#" std.toString("a") == "a" "#,
      r#" std.toString(123) == "123" "#,
      r#" std.toString(null) == "null" "#,
    ],
  },
  Fn {
    name: S::new("codepoint"),
    implemented: false,
    sig: sig(&[req("str", Ty::Str)], Ty::Uint),
    total: true,
    available_since: Some(10),
    doc: indoc! {"
      Returns the positive integer representing the unicode codepoint of the character
      in the given single-character string.

      This function is the inverse of `std.char`.
    "},
    examples: &[
      r#" std.codepoint("A") == 65 "#,
      r#" std.codepoint("a") == 97 "#,
      r#" std.codepoint("あ") == 12354 "#,
    ],
  },
  Fn {
    name: S::new("char"),
    implemented: false,
    sig: sig(&[req("n", Ty::Uint)], Ty::Str),
    total: true,
    available_since: Some(10),
    doc: indoc! {"
      Returns a string of length one whose only unicode codepoint has integer id n.

      This function is the inverse of `std.codepoint`.
    "},
    examples: &[
      r#" std.char(65) == "A" "#,
      r#" std.char(97) == "a" "#,
      r#" std.char(12354) == "あ" "#,
    ],
  },
  Fn {
    name: S::new("substr"),
    implemented: true,
    sig: sig(&[req("str", Ty::Str), req("from", Ty::Uint), req("len", Ty::Uint)], Ty::Str),
    total: false,
    available_since: Some(10),
    doc: indoc! {"
      Returns a string that is the part of `str` that starts at offset `from` and is `len`
      codepoints long.

      If the string `str` is shorter than `from + len`, returns the suffix starting at position
      `from`.
    "},
    examples: &[
      r#" std.substr("think", 1, 2) == "hi" "#,
      r#" std.substr("develop", 4, 3) == "lop" "#,
      r#" std.substr("hello world", 6, 99) == "world" "#,
    ],
  },
  Fn {
    name: S::new("findSubstr"),
    implemented: false,
    sig: sig(&[req("pat", Ty::Str), req("str", Ty::Str)], Ty::ArrNum),
    total: true,
    available_since: Some(10),
    doc: indoc! {"
      Returns an array that contains the indexes of all occurrences of `pat` in `str`.
    "},
    examples: &[
      r#" std.findSubstr("e", "envelope") == [0, 3, 7] "#,
      r#" std.findSubstr("hi", "hi Chidi") == [0, 4] "#,
      r#" std.findSubstr("fork", "shirt") == [] "#,
    ],
  },
  Fn {
    name: S::new("startsWith"),
    implemented: true,
    sig: A_B_STR_RET_BOOL,
    total: true,
    available_since: Some(10),
    doc: indoc! {r#"
      Returns whether the string `a` is prefixed by the string `b`.
    "#},
    examples: &[
      r#" std.startsWith("hi Chidi", "hi") "#,
      r#" !std.startsWith("hi Chidi", "fork") "#,
    ],
  },
  Fn {
    name: S::new("endsWith"),
    implemented: true,
    sig: A_B_STR_RET_BOOL,
    total: true,
    available_since: Some(10),
    doc: indoc! {r#"
      Returns whether the string `a` is suffixed by the string `b`.
    "#},
    examples: &[r#" std.endsWith("thank you", "you") "#, r#" !std.endsWith("thank you", "no") "#],
  },
  Fn {
    name: S::new("stripChars"),
    implemented: true,
    sig: STR_CHARS_STR_RET_STR,
    total: true,
    available_since: Some(15),
    doc: indoc! {r#"
      Removes characters from the string `chars`, treated as a set of characters, from the beginning
      and end of `str`.
    "#},
    examples: &[
      r#" std.stripChars(" test test test ", " ") == "test test test" "#,
      r#" std.stripChars("aaabbbbcccc", "ac") == "bbbb" "#,
      r#" std.stripChars("cacabbbbaacc", "ac") == "bbbb" "#,
    ],
  },
  Fn {
    name: S::new("lstripChars"),
    implemented: true,
    sig: STR_CHARS_STR_RET_STR,
    total: true,
    available_since: Some(15),
    doc: indoc! {r#"
      Removes characters from the string `chars`, treated as a set of characters, from the beginning
      of `str`.
    "#},
    examples: &[
      r#" std.lstripChars(" test test test ", " ") == "test test test " "#,
      r#" std.lstripChars("aaabbbbcccc", "ac") == "bbbbcccc" "#,
      r#" std.lstripChars("cacabbbbaacc", "ac") == "bbbbaacc" "#,
    ],
  },
  Fn {
    name: S::new("rstripChars"),
    implemented: true,
    sig: STR_CHARS_STR_RET_STR,
    total: true,
    available_since: Some(15),
    doc: indoc! {r#"
      Removes characters from the string `chars`, treated as a set of characters, from the end
      of `str`.
    "#},
    examples: &[
      r#" std.rstripChars(" test test test ", " ") == " test test test" "#,
      r#" std.rstripChars("aaabbbbcccc", "ac") == "aaabbbb" "#,
      r#" std.rstripChars("cacabbbbaacc", "ac") == "cacabbbb" "#,
    ],
  },
  Fn {
    name: S::new("split"),
    implemented: false,
    sig: sig(&[req("str", Ty::Str), req("c", Ty::Str)], Ty::ArrStr),
    total: true,
    available_since: Some(10),
    doc: indoc! {r#"
      Splits the string `str` into an array of strings, divided by the string `c`.

      Note: Versions up to and including 0.18.0 require `c` to be a single character.
    "#},
    examples: &[
      r#" std.split("foo/bar", "/") == ["foo", "bar"] "#,
      r#" std.split("/foo/bar", "/") == ["", "foo", "bar"] "#,
    ],
  },
  Fn {
    name: S::new("splitLimit"),
    implemented: false,
    sig: SPLIT_LIMIT,
    total: true,
    available_since: Some(10),
    doc: indoc! {r#"
      Identical to `std.split(str, c)`, but will stop after `maxsplits` splits, thereby the largest
      array it will return has length `maxsplits + 1`. A limit of `-1` means unlimited, and is identical to
      `std.split(str, c)`.

      Note: Versions up to and including 0.18.0 require `c` to be a single character.
    "#},
    examples: &[
      r#" std.splitLimit("foo/bar", "/", 1) == ["foo", "bar"] "#,
      r#" std.splitLimit("/foo/bar", "/", 1) == ["", "foo/bar"] "#,
    ],
  },
  Fn {
    name: S::new("splitLimitR"),
    implemented: false,
    sig: SPLIT_LIMIT,
    total: true,
    available_since: Some(19),
    doc: indoc! {r#"
      Identical to `std.splitLimit(str, c, maxsplits)`, but will split from right to left.
    "#},
    examples: &[r#" std.splitLimitR("/foo/bar", "/", 1) == ["/foo", "bar"] "#],
  },
  Fn {
    name: S::new("strReplace"),
    implemented: true,
    sig: sig(&[req("str", Ty::Str), req("from", Ty::Str), req("to", Ty::Str)], Ty::Str),
    total: true,
    available_since: Some(10),
    doc: indoc! {r#"
      Replaces all occurrences of string `from` with string `to` in `str`.
    "#},
    examples: &[indoc! {r#"
      std.strReplace("I like to skate with my skateboard", "skate", "surf")
        == "I like to surf with my surfboard"
    "#}],
  },
  Fn {
    name: S::new("isEmpty"),
    implemented: true,
    sig: sig(&[req("str", Ty::Str)], Ty::Bool),
    total: true,
    available_since: Some(20),
    doc: indoc! {r#"
      Returns whether the given string has zero length.
    "#},
    examples: &[
      r#" std.isEmpty("") "#,
      r#" !std.isEmpty("hi") "#,
      r#" !std.isEmpty("hello world") "#,
    ],
  },
  Fn {
    name: S::new("asciiUpper"),
    implemented: true,
    sig: STR_RET_STR,
    total: true,
    available_since: Some(10),
    doc: indoc! {r#"
      Returns a copy of the string in which all ASCII letters are capitalized.
    "#},
    examples: &[r#" std.asciiUpper("100 Cats!") == "100 CATS!" "#],
  },
  Fn {
    name: S::new("asciiLower"),
    implemented: true,
    sig: STR_RET_STR,
    total: true,
    available_since: Some(10),
    doc: indoc! {r#"
      Returns a copy of the string in which all ASCII letters are lower cased.
    "#},
    examples: &[r#" std.asciiLower("100 Cats!") == "100 cats!" "#],
  },
  Fn {
    name: S::new("stringChars"),
    implemented: false,
    sig: sig(&[req("str", Ty::Str)], Ty::ArrStr),
    total: true,
    available_since: Some(10),
    doc: indoc! {r#"
      Split the string into an array of strings, each containing a single codepoint.
    "#},
    examples: &[
      r#" std.stringChars("foo") == ["f", "o", "o"] "#,
      r#" std.stringChars("はい") == ["は", "い"]"#,
      r#" std.stringChars("") == []"#,
    ],
  },
  Fn {
    name: S::new("format"),
    implemented: false,
    sig: sig(&[req("str", Ty::Str), req("vals", Ty::Any)], Ty::Str),
    total: true,
    available_since: Some(10),
    doc: indoc! {r#"
      Formats the string `str` using the values in `vals`.

      The `vals` can be an array, an object, or in other cases are treated as if they were provided
      in a singleton array.

      The string formatting follows the same rules as Python.

      The `%` operator can be used as a shorthand for this function.
    "#},
    examples: &[
      r#" std.format("Hello %03d", 12) == "Hello 012" "#,
      r#" "Hello %03d" % 12 == "Hello 012" "#,
      r#" "Hello %s, age %d" % ["Foo", 25] == "Hello Foo, age 25" "#,
      indoc! {r#"
      "Hello %(name)s, age %(age)d" % {age: 25, name: "Foo"}
        == "Hello Foo, age 25"
      "#},
    ],
  },
  Fn {
    name: S::new("escapeStringBash"),
    implemented: false,
    sig: STR_RET_STR,
    total: true,
    available_since: Some(10),
    doc: indoc! {r#"
      Wraps the string in single quotes, and escapes any single quotes within `str` by changing them
      to a sequence `'"'"'`.

      This allows injection of arbitrary strings as arguments of commands in bash scripts.
    "#},
    examples: &[r#" std.escapeStringBash("echo 'hi'") == "'echo '\"'\"'hi'\"'\"''" "#],
  },
  Fn {
    name: S::new("escapeStringDollars"),
    implemented: false,
    sig: STR_RET_STR,
    total: true,
    available_since: Some(10),
    doc: indoc! {"
      Converts `$` to `$$` in the string.

      This allows injection of arbitrary strings into systems that use `$` for string
      interpolation, like Terraform.
    "},
    examples: &[r#" std.escapeStringDollars("1 + $x") == "1 + $$x" "#],
  },
  Fn {
    name: S::new("escapeStringJson"),
    implemented: false,
    sig: STR_RET_STR,
    total: true,
    available_since: Some(10),
    doc: indoc! {r#"
      Converts the string to allow it to be embedded in a JSON representation, within a string.

      This adds quotes, escapes backslashes, and escapes unprintable characters.
    "#},
    examples: &[indoc! {r#"
      "{name: %s}" % std.escapeStringJson("Multiline\nc:\\path")
        == "{name: \"Multiline\\nc:\\\\path\"}"
    "#}],
  },
  Fn {
    name: S::new("escapeStringPython"),
    implemented: false,
    sig: STR_RET_STR,
    total: true,
    available_since: Some(10),
    doc: indoc! {"
      Converts the string to allow it to be embedded in Python.

      This is an alias for `escapeStringJson`.
    "},
    examples: &[indoc! {r#"
      "{name: %s}" % std.escapeStringPython("Multiline\nc:\\path")
        == "{name: \"Multiline\\nc:\\\\path\"}"
    "#}],
  },
  Fn {
    name: S::new("escapeStringXml"),
    implemented: false,
    sig: STR_RET_STR,
    total: true,
    available_since: Some(10),
    doc: indoc! {r#"
      Converts the string to allow it to be embedded in XML (or HTML). The following replacements
      are made:

      | Replace | With     |
      | ------- | -------- |
      | `<`     | `&lt;`   |
      | `>`     | `&gt;`   |
      | `&`     | `&amp;`  |
      | `"`     | `&quot;` |
      | `'`     | `&apos;` |
    "#},
    examples: &[
      r#" std.escapeStringXml("2 > 1") == "2 &gt; 1" "#,
      r#" std.escapeStringXml("that's great") == "that&apos;s great" "#,
    ],
  },
  Fn {
    name: S::new("parseInt"),
    implemented: false,
    sig: STR_RET_NUM,
    total: true,
    available_since: Some(10),
    doc: indoc! {r#"
      Parses a signed decimal integer from the input string.
    "#},
    examples: &[r#" std.parseInt("123") == 123 "#, r#" std.parseInt("-123") == -123 "#],
  },
  Fn {
    name: S::new("parseOctal"),
    implemented: false,
    sig: STR_RET_NUM,
    total: true,
    available_since: Some(10),
    doc: indoc! {r#"
      Parses an unsigned octal integer from the input string. Initial zeroes are tolerated.
    "#},
    examples: &[r#" std.parseOctal("755") == 493 "#, r#" std.parseOctal("0755") == 493 "#],
  },
  Fn {
    name: S::new("parseHex"),
    implemented: false,
    sig: STR_RET_NUM,
    total: true,
    available_since: Some(10),
    doc: indoc! {r#"
      Parses an unsigned hexadecimal integer, from the input string. Case insensitive.
    "#},
    examples: &[r#" std.parseHex("ff") == 255 "#, r#" std.parseHex("BADc0de") == 195936478 "#],
  },
  Fn {
    name: S::new("parseJson"),
    implemented: false,
    sig: STR_RET_ANY,
    total: true,
    available_since: Some(13),
    doc: indoc! {r#"
      Parses a JSON string.
    "#},
    examples: &[
      r#" std.parseJson("null") == null "#,
      r#" std.parseJson("[2, false]") == [2, false] "#,
      r#" std.parseJson('{"foo": "bar"}') == { "foo": "bar" } "#,
    ],
  },
  Fn {
    name: S::new("parseYaml"),
    implemented: false,
    sig: STR_RET_ANY,
    total: true,
    available_since: Some(18),
    doc: indoc! {r#"
      Parses a YAML string.

      This is provided as a "best-effort" mechanism and should not be relied on to provide a fully
      standards compliant YAML parser.

      YAML is a superset of JSON, consequently "downcasting" or manifestation of YAML into JSON or
      Jsonnet values will only succeed when using the subset of YAML that is compatible with JSON.

      The parser does not support YAML documents with scalar values at the root. The root node of a
      YAML document must start with either a YAML sequence or map to be successfully parsed.
    "#},
    examples: &[r#" std.parseYaml("foo: bar") == { "foo": "bar" } "#],
  },
  Fn {
    name: S::new("encodeUTF8"),
    implemented: false,
    sig: sig(&[req("str", Ty::Str)], Ty::ArrNum),
    total: true,
    available_since: Some(13),
    doc: indoc! {"
      Encode a string using UTF8. Returns an array of numbers representing bytes.
    "},
    examples: &[r#" std.encodeUTF8("hey") == [104, 101, 121] "#],
  },
  Fn {
    name: S::new("decodeUTF8"),
    implemented: false,
    sig: sig(&[req("arr", Ty::ArrNum)], Ty::Str),
    total: true,
    available_since: Some(13),
    doc: indoc! {"
      Decode an array of numbers representing bytes using UTF8. Returns a string.
    "},
    examples: &[r#" std.decodeUTF8([104, 101, 121]) == "hey" "#],
  },
  Fn {
    name: S::new("manifestIni"),
    implemented: false,
    sig: sig(&[req("ini", Ty::Obj)], Ty::Str),
    total: true,
    available_since: Some(10),
    doc: indoc! {r#"
      Convert the given structure to a string in INI format.

      This allows using Jsonnet's object model to build a configuration to be consumed by an
      application expecting an INI file. The data is in the form of a set of sections, each
      containing a key/value mapping.

      This example:

      ```jsonnet
      std.manifestIni({
        main: { a: "1", b: "2" },
        sections: {
          s1: { x: "11", y: "22", z: "33" },
          s2: { p: "yes", q: ""},
          empty: {},
        }
      })
      ```

      Yields a string containing this INI file:

      ```ini
      a = 1
      b = 2
      [empty]
      [s1]
      x = 11
      y = 22
      z = 33
      [s2]
      p = yes
      q =
      ```
    "#},
    examples: &[],
  },
  Fn {
    name: S::new("manifestPython"),
    implemented: false,
    sig: sig(&[req("v", Ty::Any)], Ty::Str),
    total: true,
    available_since: Some(10),
    doc: indoc! {r#"
      Convert the given value to a JSON-like form that is compatible with Python.

      The chief differences are:

      | Replace | With    |
      | ------- | ------- |
      | `true`  | `True`  |
      | `false` | `False` |
      | `null`  | `None`  |

      This example:

      ```jsonnet
      std.manifestPython({
        b: ["foo", "bar"],
        c: true,
        d: null,
        e: { f1: false, f2: 42 },
      })
      ```

      Yields a string containing Python code like:

      ```py
      {
        "b": ["foo", "bar"],
        "c": True,
        "d": None,
        "e": { "f1": False, "f2": 42 }
      }
      ```
    "#},
    examples: &[],
  },
  Fn {
    name: S::new("manifestPythonVars"),
    implemented: false,
    sig: sig(&[req("conf", Ty::Any)], Ty::Str),
    total: true,
    available_since: Some(10),
    doc: indoc! {r#"
      Convert the given object to a JSON-like form that is compatible with Python.

      The key difference to `std.manifestPython` is that the top level is represented as a list of
      Python global variables.

      This example:

      ```jsonnet
      std.manifestPythonVars({
        b: ["foo", "bar"],
        c: true,
        d: null,
        e: { f1: false, f2: 42 },
      })
      ```

      Yields a string containing this Python code:

      ```py
      b = ["foo", "bar"]
      c = True
      d = None
      e = {"f1": False, "f2": 42}
      ```
    "#},
    examples: &[],
  },
  Fn {
    name: S::new("manifestJsonEx"),
    implemented: false,
    sig: sig(
      &[
        req("value", Ty::Any),
        req("indent", Ty::Str),
        opt("newline", Ty::Str, "\"\\n\""),
        opt("key_val_sep", Ty::Str, ": "),
      ],
      Ty::Str,
    ),
    total: true,
    available_since: Some(10),
    doc: indoc! {r#"
      Converts the given object to a JSON form.

      `indent` is a string containing one or more whitespace characters that are used for
      indentation.

      `newline` is inserted where a newline would normally be used to break long lines.

      `key_val_sep` is used to separate the key and value of an object field.
    "#},
    examples: &[],
  },
  Fn {
    name: S::new("manifestJson"),
    implemented: false,
    sig: MANIFEST_JSON,
    total: true,
    available_since: Some(10),
    doc: indoc! {"
      Convert the given object to a JSON form.

      Under the covers, it calls `std.manifestJsonEx` with a 4-space indent.
    "},
    examples: &[],
  },
  Fn {
    name: S::new("manifestJsonMinified"),
    implemented: false,
    sig: MANIFEST_JSON,
    total: true,
    available_since: Some(18),
    doc: indoc! {"
      Convert the given object to a minified JSON form, with no extra whitespace.

      Under the covers, it calls `std.manifestJsonEx`.
    "},
    examples: &[],
  },
  Fn {
    name: S::new("manifestYamlDoc"),
    implemented: false,
    sig: sig(
      &[
        req("value", Ty::Any),
        opt("indent_array_in_object", Ty::Bool, "false"),
        opt("quote_keys", Ty::Bool, "true"),
      ],
      Ty::Str,
    ),
    total: true,
    available_since: Some(10),
    doc: indoc! {r#"
      Converts the given value to a YAML form.

      Note that `manifestJson` could also be used for this purpose, because any JSON is also valid
      YAML. But this function will produce more canonical-looking YAML.

      The `indent_array_in_object` parameter adds additional indentation which some people may find
      easier to read.

      The `quote_keys` parameter controls whether YAML identifiers are always quoted or only when
      necessary.

      This example:

      ```jsonnet
      std.manifestYamlDoc(
        {
          x: [1, 2, 3, true, false, null, "string\nstring\n"],
          y: { a: 1, b: 2, c: [1, 2] },
        },
        indent_array_in_object=false,
      )
      ```

      Yields a string containing this YAML:

      ```yaml
      "x":
        - 1
        - 2
        - 3
        - true
        - false
        - null
        - |
          string
          string
      "y":
        "a": 1
        "b": 2
        "c":
          - 1
          - 2
      ```
    "#},
    examples: &[],
  },
  Fn {
    name: S::new("manifestYamlStream"),
    implemented: false,
    sig: sig(
      &[
        req("value", Ty::ArrAny),
        opt("indent_array_in_object", Ty::Bool, "false"),
        opt("c_document_end", Ty::Bool, "false"),
        opt("quote_keys", Ty::Bool, "true"),
      ],
      Ty::Str,
    ),
    total: true,
    available_since: Some(10),
    doc: indoc! {r#"
      Given an array of values,

      ```
      std.manifestYamlStream(
        value,
        indent_array_in_object=false,
        c_document_end=false,
        quote_keys=true,
      )
      ```

      emits a YAML "stream", which is a sequence of documents separated by `---` and ending with
      `...`.

      The `indent_array_in_object` and `quote_keys` params are the same as in `std.manifestYamlDoc`.

      The `c_document_end` param adds the optional terminating `...`.

      This example:

      ```jsonnet
      std.manifestYamlStream( ["a", 1, []], indent_array_in_object=false, c_document_end=true)
      ```

      Yields this string:

      ```yaml
      ---
      "a"
      ---
      1
      ---
      []
      ```
    "#},
    examples: &[],
  },
  Fn {
    name: S::new("manifestXmlJsonml"),
    implemented: false,
    sig: sig(&[req("value", Ty::ArrAny)], Ty::Str),
    total: true,
    available_since: Some(10),
    doc: indoc! {r#"
      Convert the given [JsonML](http://www.jsonml.org)-encoded value to a string containing the
      XML.

      This example:

      ```jsonnet
      std.manifestXmlJsonml([
        "svg", { height: 100, width: 100 },
        [
          "circle", {
            cx: 50, cy: 50, r: 40,
            stroke: "black", "stroke-width": 3,
            fill: "red",
          }
        ],
      ])
      ```

      Yields a string containing this XML (all on one line):

      ```xml
      <svg height="100" width="100">
        <circle
          cx="50"
          cy="50"
          r="40"
          fill="red"
          stroke="black"
          stroke-width="3"
        ></circle>
      </svg>
      ```

      JsonML is designed to preserve "mixed-mode content" (i.e., textual data outside of or next to
      elements). This includes the whitespace needed to avoid having all the XML on one line, which
      is meaningful in XML. In order to have whitespace in the XML output, it must be present in the
      JsonML input:

      ```jsonnet
      std.manifestXmlJsonml([
        "svg",
        { height: 100, width: 100 },
        "\n  ",
        [
          "circle", {
            cx: 50, cy: 50, r: 40, stroke: "black",
            "stroke-width": 3, fill: "red",
          }
        ],
        "\n",
      ])
      ```
    "#},
    examples: &[],
  },
  Fn {
    name: S::new("manifestTomlEx"),
    implemented: false,
    sig: sig(&[req("toml", Ty::Obj), req("indent", Ty::Str)], Ty::Str),
    total: true,
    available_since: None,
    doc: indoc! {"
      Converts the given `toml` to a TOML form.

      `indent` is a string containing one or more whitespace characters that are used for
      indentation.
    "},
    examples: &[],
  },
  Fn {
    name: S::new("makeArray"),
    implemented: false,
    sig: sig(&[req("sz", Ty::Uint), req("func", Ty::Hof1)], Ty::ArrAny),
    total: true,
    available_since: Some(10),
    doc: indoc! {"
      Creates a new array of `sz` elements by calling `func` to initialize each element.

      `func` is a function that takes a single parameter, the index of the element it should
      initialize.
    "},
    examples: &[r#" std.makeArray(3, function(x) x * x) == [0, 1, 4] "#],
  },
  Fn {
    name: S::new("member"),
    implemented: false,
    sig: sig(&[req("arr", Ty::StrOrArrAny), req("x", Ty::Any)], Ty::Bool),
    total: true,
    available_since: Some(15),
    doc: indoc! {"
      Returns whether `x` occurs in `arr`.

      Argument `arr` may be an array or a string.
    "},
    examples: &[
      r#" std.member([1, 2], 2) "#,
      r#" !std.member([1, 2], 3) "#,
      r#" std.member("hello", "l") "#,
      r#" !std.member("hello", "z") "#,
    ],
  },
  Fn {
    name: S::new("count"),
    implemented: false,
    sig: sig(&[req("arr", Ty::ArrAny), req("x", Ty::Any)], Ty::Num),
    total: true,
    available_since: Some(10),
    doc: indoc! {"
      `std.count(arr, x)` returns the number of times that `x` occurs in `arr`.
    "},
    examples: &[
      r#" std.count([1, 2], 2) == 1 "#,
      r#" std.count([1, 2], 3) == 0 "#,
      r#" std.count([3, 1, 3, 0], 3) == 2 "#,
    ],
  },
  Fn {
    name: S::new("find"),
    implemented: false,
    sig: sig(&[req("value", Ty::Any), req("arr", Ty::ArrAny)], Ty::ArrNum),
    total: true,
    available_since: Some(10),
    doc: indoc! {"
      Returns an array that contains the indexes of all occurrences of `value` in `arr`.
    "},
    examples: &[
      r#" std.find(2, [1, 2]) == [0] "#,
      r#" std.find(3, [1, 2]) == [] "#,
      r#" std.find(3, [3, 1, 3, 0]) == [0, 2] "#,
    ],
  },
  Fn {
    name: S::new("map"),
    implemented: false,
    sig: ARR_HOF1,
    total: true,
    available_since: Some(10),
    doc: indoc! {"
      Applies the given `func` to every element of `arr` to form a new array.
    "},
    examples: &[
      r#" std.map(function(x) x + 1, [2, 4]) == [3, 5] "#,
      r#" std.map(function(x) error "oh no", []) "#,
    ],
  },
  Fn {
    name: S::new("mapWithIndex"),
    implemented: false,
    sig: sig(&[req("func", Ty::Hof2), req("arr", Ty::ArrAny)], Ty::ArrAny),
    total: true,
    available_since: Some(10),
    doc: indoc! {"
      Similar to `std.map`, but it also passes to the function the element's index in the array.

      The function is expected to take the index as the first parameter and the element as the
      second.
    "},
    examples: &[
      r#" std.mapWithIndex(function(i, x) x + i, [2, 4]) == [2, 5] "#,
      r#" std.mapWithIndex(function(i, x) error "oh no", []) "#,
    ],
  },
  Fn {
    name: S::new("filterMap"),
    implemented: false,
    sig: sig(
      &[req("filter_func", Ty::Hof1), req("map_func", Ty::Hof1), req("arr", Ty::ArrAny)],
      Ty::ArrAny,
    ),
    total: true,
    available_since: Some(10),
    doc: indoc! {"
      First filters with `filter_func`, then maps with `map_func`, the given array `arr`.
    "},
    examples: &[indoc! {"
      std.filterMap(
        function(x) x > 3,
        function(x) x + 2,
        [1, 6, 2, 5],
      ) == [
        8,
        7,
      ]
    "}],
  },
  Fn {
    name: S::new("flatMap"),
    implemented: false,
    sig: sig(&[req("func", Ty::Hof1), req("arr", Ty::StrOrArrAny)], Ty::StrOrArrAny),
    total: true,
    available_since: Some(10),
    doc: indoc! {r#"
      Applies the given function to every element of `arr`, then flattens the result.

      The argument `arr` must be an array or a string.

      - If `arr` is an array, `func` must return an array.
      - If `arr` is a string, `func` must return a string.

      Can be thought of as a generalized `map`, with each element mapped to 0, 1 or more elements.
    "#},
    examples: &[
      indoc! {"
        std.flatMap(function(x) [x, x], [1, 2, 3])
          == [1, 1, 2, 2, 3, 3]
      "},
      indoc! {"
        std.flatMap(function(x) if x == 2 then [] else [x], [1, 2, 3])
          == [1, 3]
      "},
      indoc! {"
        std.flatMap(function(x) if x == 2 then [] else [x * 3, x * 2], [1, 2, 3])
          == [3, 2, 9, 6]
      "},
      indoc! {r#"
        std.flatMap(function(x) x + x, "foo")
          == "ffoooo"
      "#},
    ],
  },
  Fn {
    name: S::new("filter"),
    implemented: false,
    sig: ARR_HOF1,
    total: true,
    available_since: Some(10),
    doc: indoc! {"
      Returns a new array containing all the elements of `arr` for which the `func` function returns
      `true`.
    "},
    examples: &[" std.filter(function(x) x > 3, [1, 6, 2, 8, 3, 8]) == [6, 8, 8] "],
  },
  Fn {
    name: S::new("foldl"),
    implemented: false,
    sig: FOLD,
    total: true,
    available_since: Some(10),
    doc: indoc! {r#"
      Calls the function `func` on the result of the previous function call and each array element
      of `arr`, or `init` in the case of the initial element.

      Traverses `arr` from left to right.
    "#},
    examples: &[indoc! {r#"
      std.foldl(function(ac, x) "(%s %s)" % [ac, x], ["a", "b", "c"], "_")
        == "(((_ a) b) c)"
    "#}],
  },
  Fn {
    name: S::new("foldr"),
    implemented: false,
    sig: FOLD,
    total: true,
    available_since: Some(10),
    doc: indoc! {r#"
      Calls the function `func` on the result of the previous function call and each array element
      of `arr`, or `init` in the case of the initial element.

      Traverses `arr` from right to left.
    "#},
    examples: &[indoc! {r#"
      std.foldr(function(ac, x) "(%s %s)" % [ac, x], ["a", "b", "c"], "_")
        == "(((_ c) b) a)"
    "#}],
  },
  Fn {
    name: S::new("range"),
    implemented: false,
    sig: sig(&[req("from", Ty::Num), req("to", Ty::Num)], Ty::ArrNum),
    total: true,
    available_since: Some(10),
    doc: indoc! {"
      Returns an array of ascending numbers between `from` and `to`, inclusively.

      Raises if `from` or `to` are not integers.
    "},
    examples: &[
      r#" std.range(2, 6) == [2, 3, 4, 5, 6] "#,
      r#" std.range(-1, 3) == [-1, 0, 1, 2, 3] "#,
      r#" std.range(0, 0) == [0] "#,
    ],
  },
  Fn {
    name: S::new("repeat"),
    implemented: false,
    sig: sig(&[req("what", Ty::StrOrArrAny), req("count", Ty::Uint)], Ty::StrOrArrAny),
    total: true,
    available_since: Some(15),
    doc: indoc! {r#"
      Repeats an array or a string `what` a number of times specified by an integer `count`.
    "#},
    examples: &[
      r#" std.repeat([1, 2], 3) == [1, 2, 1, 2, 1, 2] "#,
      r#" std.repeat("boo", 2) == "booboo" "#,
    ],
  },
  Fn {
    name: S::new("slice"),
    implemented: false,
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
    available_since: Some(10),
    doc: indoc! {r#"
      Selects the elements of `indexable`, an array or a string, from `index` to `end` with `step`,
      and returns an array or a string respectively.

      Note that it's recommended to use dedicated slicing syntax both for arrays and strings,
      e.g. `arr[0:4:1]` instead of `std.slice(arr, 0, 4, 1)`.
    "#},
    examples: &[
      r#" std.slice([1, 2, 3, 4, 5, 6], 0, 4, 1) == [1, 2, 3, 4] "#,
      r#" std.slice([1, 2, 3, 4, 5, 6], 1, 6, 2) == [2, 4, 6] "#,
      r#" std.slice("jsonnet", 0, 4, 1) == "json" "#,
      r#" std.slice("jsonnet", -3, null, null) == "net" "#,
    ],
  },
  Fn {
    name: S::new("join"),
    implemented: true,
    sig: sig(&[req("sep", Ty::StrOrArrAny), req("arr", Ty::ArrAny)], Ty::StrOrArrAny),
    total: false,
    available_since: Some(10),
    doc: indoc! {r#"
      Joins the elements of `arr` together, each separated by `sep`.

      If `sep` is a string, then `arr` must be an array of strings, in which case they are
      concatenated with `sep` used as a delimiter.

      If `sep` is an array, then `arr` must be an array of arrays, in which case the arrays are
      concatenated in the same way, to produce a single array.
    "#},
    examples: &[
      r#" std.join(".", ["www", "google", "com"]) == "www.google.com" "#,
      r#" std.join([9, 9], [[1], [2, 3]]) == [ 1, 9, 9, 2, 3 ] "#,
    ],
  },
  Fn {
    name: S::new("lines"),
    implemented: false,
    sig: sig(&[req("arr", Ty::ArrStr)], Ty::Str),
    total: true,
    available_since: Some(10),
    doc: indoc! {"
      Concatenate an array of strings into a text file with newline characters after each string.

      This is suitable for constructing bash scripts and the like.
    "},
    examples: &[
      indoc! {r#"
        std.lines([
          "cd /tmp",
          "ls -l",
          "mkdir -p foo",
        ]) == "cd /tmp\nls -l\nmkdir -p foo\n"
      "#},
      r#" std.lines([]) == "" "#,
    ],
  },
  Fn {
    name: S::new("flattenArrays"),
    implemented: false,
    sig: sig(&[req("arr", Ty::ArrAny)], Ty::ArrAny),
    total: true,
    available_since: Some(10),
    doc: indoc! {"
      Concatenate an array of arrays into a single array.

      Does not recursively flatten deeply nested arrays.
    "},
    examples: &[indoc! {"
      std.flattenArrays([[1, 2], [3], [[4], [5, 6]]])
        == [1, 2, 3, [4], [5, 6]]
      "}],
  },
  Fn {
    name: S::new("reverse"),
    implemented: false,
    sig: sig(&[req("arr", Ty::ArrAny)], Ty::ArrAny),
    total: true,
    available_since: Some(13),
    doc: indoc! {"
      Returns the argument array reversed.
    "},
    examples: &[
      "std.reverse([2, 4, 6]) == [6, 4, 2]",
      "std.reverse([8]) == [8]",
      "std.reverse([]) == []",
    ],
  },
  Fn {
    name: S::new("sort"),
    implemented: false,
    sig: ARR_KEY_F,
    total: true,
    available_since: Some(10),
    doc: indoc! {r#"
      Sorts the array using the `<=` operator.

      The optional argument `keyF` is a single argument function used to extract comparison key from
      each array element.

      ```jsonnet
      assert std.sort([5, 2, 9]) == [2, 5, 9];

      local fellas = [
        { name: "fred", age: 5 },
        { name: "george", age: 8 },
        { name: "ringo", age: 3 },
      ];
      local getAge(x) = x.age;
      assert std.sort(fellas, keyF=getAge) == [
        { name: "ringo", age: 3 },
        { name: "fred", age: 5 },
        { name: "george", age: 8 },
      ];
      ```
    "#},
    examples: &[],
  },
  Fn {
    name: S::new("uniq"),
    implemented: false,
    sig: ARR_KEY_F,
    total: true,
    available_since: Some(10),
    doc: indoc! {"
      Removes successive duplicates.

      When given a sorted array, removes all duplicates.

      The optional argument `keyF` is a single argument function used to extract comparison key from
      each array element.
    "},
    examples: &["std.uniq([1, 1, 1]) == [1]", "std.uniq([1, 2, 2, 3, 2]) == [1, 2, 3, 2]"],
  },
  Fn {
    name: S::new("all"),
    implemented: false,
    sig: sig(&[req("arr", Ty::ArrBool)], Ty::Bool),
    total: true,
    available_since: Some(19),
    doc: indoc! {"
      Returns whether all elements of the input array are `true`.

      Raises if `arr` is not an array, or `arr` contains non-boolean values.
    "},
    examples: &[
      "std.all([true, true]) == true",
      "std.all([true, false]) == false",
      "std.all([true, true, true, false, true]) == false",
      "std.all([]) == true",
    ],
  },
  Fn {
    name: S::new("any"),
    implemented: false,
    sig: sig(&[req("arr", Ty::ArrBool)], Ty::Bool),
    total: true,
    available_since: Some(19),
    doc: indoc! {"
      Returns whether any element of `arr` is `true`.

      Raises if `arr` is not an array, or `arr` contains non-boolean values.
    "},
    examples: &[
      "std.any([false, false]) == false",
      "std.any([true, false]) == true",
      "std.any([false, false, false, true, false]) == true",
      "std.any([]) == false",
    ],
  },
  Fn {
    name: S::new("sum"),
    implemented: false,
    sig: sig(&[req("arr", Ty::ArrNum)], Ty::Num),
    total: true,
    available_since: Some(20),
    doc: indoc! {"
      Returns the sum of all the elements.
    "},
    examples: &["std.sum([]) == 0", "std.sum([1, 2, 3, 4]) == 10"],
  },
  Fn {
    name: S::new("avg"),
    implemented: false,
    sig: sig(&[req("arr", Ty::ArrNum)], Ty::Num),
    total: true,
    available_since: Some(20),
    doc: indoc! {"
      Returns the average of all the elements.

      Raises if the input is empty.
    "},
    examples: &[
      "std.avg([1, 3]) == 2",
      "std.avg([2, 4, 6, 5, 6, 7]) == 5",
      "std.avg([1, 2]) == 1.5",
    ],
  },
  Fn {
    name: S::new("set"),
    implemented: false,
    sig: ARR_KEY_F,
    total: true,
    available_since: Some(10),
    doc: indoc! {"
      Returns a sorted array with no duplicates, i.e. a set.

      It is equivalent to `std.uniq(std.sort(arr))`.

      The optional `keyF` function can be used to extract a key to use from each element. This key
      is used for the purpose of identifying uniqueness.
    "},
    examples: &[r#" std.set([1, 6, 2, 6]) == [1, 2, 6] "#],
  },
  Fn {
    name: S::new("setInter"),
    implemented: false,
    sig: BINARY_SET_FN,
    total: true,
    available_since: Some(10),
    doc: indoc! {"
      Returns the set containing values that are in both `a` and `b`.

      `a` and `b` must be sets, i.e. sorted arrays with no duplicates. If that is not the case, this
      function will quietly return non-meaningful results.

      The optional `keyF` function can be used to extract a key to use from each element. This key
      is used for the purpose of identifying uniqueness.
    "},
    examples: &[r#" std.setInter([1, 2], [2, 3]) == [2] "#],
  },
  Fn {
    name: S::new("setUnion"),
    implemented: false,
    sig: BINARY_SET_FN,
    total: true,
    available_since: Some(10),
    doc: indoc! {r#"
      Returns the set containing values that are in either `a` or `b`.

      Note that `+` on sets will simply concatenate the arrays, possibly forming an array that is
      not a set (due to not being ordered without duplicates).

      `a` and `b` must be sets, i.e. sorted arrays with no duplicates. If that is not the case, this
      function will quietly return non-meaningful results.

      The optional `keyF` function can be used to extract a key to use from each element. This key
      is used for the purpose of identifying uniqueness.
    "#},
    examples: &[
      "std.setUnion([1, 2], [2, 3]) == [1, 2, 3]",
      indoc! {r#"
        std.setUnion(
          [{n:"A", v:1}, {n:"B"}],
          [{n:"A", v: 9999}, {n:"C"}],
          keyF=function(x) x.n
        ) == [{ "n": "A", "v": 1 }, { "n": "B" }, { "n": "C" }]
      "#},
    ],
  },
  Fn {
    name: S::new("setDiff"),
    implemented: false,
    sig: BINARY_SET_FN,
    total: true,
    available_since: Some(10),
    doc: indoc! {"
      Returns the set containing values that are in `a` but not `b`.

      `a` and `b` must be sets, i.e. sorted arrays with no duplicates. If that is not the case, this
      function will quietly return non-meaningful results.

      The optional `keyF` function can be used to extract a key to use from each element. This key
      is used for the purpose of identifying uniqueness.
    "},
    examples: &[r#" std.setDiff([1, 2], [2, 3]) == [1] "#],
  },
  Fn {
    name: S::new("setMember"),
    implemented: false,
    sig: sig(&[req("x", Ty::Any), req("arr", Ty::ArrAny), KEY_F], Ty::Bool),
    total: true,
    available_since: Some(10),
    doc: indoc! {"
      Returns whether `x` is a member of `s`.

      `s` must be a set, i.e. a sorted array with no duplicates. If that is not the case, this
      function will quietly return non-meaningful results.

      The optional `keyF` function can be used to extract a key to use from each element. This key
      is used for the purpose of identifying uniqueness.
    "},
    examples: &[r#" std.setMember(1, [1, 2]) "#, r#" !std.setMember(3, [1, 2]) "#],
  },
  Fn {
    name: S::new("base64"),
    implemented: false,
    sig: sig(&[req("input", Ty::StrOrArrNum)], Ty::Str),
    total: true,
    available_since: Some(10),
    doc: indoc! {"
      Encodes the given value into a base64 string.

      The encoding sequence is `A-Za-z0-9+/` with `=` to pad the output to a multiple of 4
      characters.

      The value can be a string or an array of numbers, but the codepoints / numbers must be in the
      0 to 255 range.

      The resulting string has no line breaks.
    "},
    examples: &[r#" std.base64("hello world") == "aGVsbG8gd29ybGQ=" "#],
  },
  Fn {
    name: S::new("base64DecodeBytes"),
    implemented: false,
    sig: sig(&[req("str", Ty::Str)], Ty::ArrNum),
    total: true,
    available_since: Some(10),
    doc: indoc! {"
      Decodes the given base64 string into an array of bytes (number values).

      Currently assumes the input string has no line breaks and is padded to a multiple of 4
      (with the `=` character). In other words, it consumes the output of `base64`.
    "},
    examples: &[r#" std.decodeUTF8(std.base64DecodeBytes("aGVsbG8gd29ybGQ=")) == "hello world" "#],
  },
  Fn {
    name: S::new("base64Decode"),
    implemented: false,
    sig: STR_RET_STR,
    total: true,
    available_since: Some(10),
    doc: indoc! {"
      **Deprecated**: use `std.base64DecodeBytes` and decode the string explicitly
      (e.g. with `std.decodeUTF8`) instead.

      Behaves like `std.base64DecodeBytes` except returns a naively encoded string instead of an
      array of bytes.
    "},
    examples: &[],
  },
  Fn {
    name: S::new("md5"),
    implemented: false,
    sig: sig(&[req("s", Ty::Str)], Ty::Str),
    total: true,
    available_since: Some(10),
    doc: indoc! {"
      Encodes the given value into an MD5 string.
    "},
    examples: &[r#" std.md5("hello world") == "5eb63bbbe01eeed093cb22bb8f5acdc3" "#],
  },
  Fn {
    name: S::new("xor"),
    implemented: true,
    sig: X_Y_BOOL_RET_BOOL,
    total: true,
    available_since: Some(20),
    doc: indoc! {"
      Returns the xor (exclusive or) of the two given booleans.
    "},
    examples: &[
      "!std.xor(true, true)",
      "std.xor(true, false)",
      "std.xor(false, true)",
      "!std.xor(false, false)",
    ],
  },
  Fn {
    name: S::new("xnor"),
    implemented: true,
    sig: X_Y_BOOL_RET_BOOL,
    total: true,
    available_since: Some(20),
    doc: indoc! {"
      Returns the xnor (exclusive nor) of the two given booleans.
    "},
    examples: &[
      "std.xnor(true, true)",
      "!std.xnor(true, false)",
      "!std.xnor(false, true)",
      "std.xnor(false, false)",
    ],
  },
  Fn {
    name: S::new("mergePatch"),
    implemented: false,
    sig: sig(&[req("target", Ty::Any), req("patch", Ty::Any)], Ty::Any),
    total: true,
    available_since: Some(10),
    doc: indoc! {"
      Applies `patch` to `target` according to
      [RFC7396](https://tools.ietf.org/html/rfc7396).
    "},
    examples: &[
      r#" std.mergePatch({ a: 1, b: 2 }, "hi") == "hi" "#,
      indoc! {"
        std.mergePatch(
          { a: 1, b: 2 },
          { a: 3, c: 4 },
        ) == {
          a: 3,
          b: 2,
          c: 4,
        }
      "},
    ],
  },
  Fn {
    name: S::new("trace"),
    implemented: false,
    sig: sig(&[req("str", Ty::Str), req("rest", Ty::Any)], Ty::Any),
    total: true,
    available_since: Some(11),
    doc: indoc! {r#"
      Outputs the given string `str` to stderr and returns `rest`.

      ```jsonnet
      local choose(c, yes, no) =
        if c then
          std.trace("c is true, returning " + std.toString(yes), yes)
        else
          std.trace("c is false, returning " + std.toString(no), no);

      {
        foo: choose(true, { bar: 1 }, { quz: 2 }),
      }
      ```

      Prints:

      ```text
      TRACE: test.jsonnet:3 c is true, returning {"bar": 1}
      ```

      And evaluates to:

      ```json
      {
        "foo": {
          "bar": 1
        }
      }
      ```
    "#},
    examples: &[],
  },
  // alluded to in the spec but not mentioned on the std lib page
  Fn {
    name: S::new("equals"),
    implemented: true,
    sig: sig(&[req("x", Ty::Any), req("y", Ty::Any)], Ty::Bool),
    total: false,
    available_since: None,
    doc: indoc! {"
      Returns whether the two arguments equal each other.
    "},
    examples: &["std.equals(1 + 1, 2)", "!std.equals(2 + 2, 5)"],
  },
  Fn {
    name: S::new("objectHasEx"),
    implemented: false,
    sig: sig(&[req("obj", Ty::Obj), req("fname", Ty::Str), req("hidden", Ty::Bool)], Ty::Bool),
    total: true,
    available_since: None,
    doc: indoc! {"
      Identical to:

      - `std.objectHasAll(obj, fname)` when `hidden` is `true`;
      - `std.objectHas(obj, fname)` when `hidden` is `false`.
    "},
    examples: &[
      r#" std.objectHasEx({a:: 1}, "a", true) "#,
      r#" !std.objectHasEx({a:: 1}, "a", false) "#,
    ],
  },
];
