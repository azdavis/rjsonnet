//! Info about the standard library.
//!
//! Based on [the original std lib docs](https://jsonnet.org/ref/stdlib.html).

#![allow(clippy::needless_raw_string_hashes)]

#[cfg(test)]
mod tests;

pub mod examples;

use examples::Examples;
use indoc::indoc;
use std::fmt;

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
  /// Some examples to show in the doc. Each element is a Jsonnet expression that should evaluate to
  /// `true`.
  pub examples: Examples,
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
  pub default: Option<ParamDefault>,
}

impl Param {
  /// Returns whether this parameter is required.
  #[must_use]
  pub fn is_required(&self) -> bool {
    self.default.is_none()
  }
}

/// A parameter default.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ParamDefault {
  /// The identity function.
  IdentityFn,
  /// `null`
  Null,
  /// `true`
  True,
  /// `false`
  False,
  /// The string of length 1 whose single char is the newline char.
  NewlineChar,
  /// A string composed of a colon and a space.
  ColonSpace,
}

impl ParamDefault {
  /// Returns this as a static str.
  #[must_use]
  pub fn as_static_str(self) -> &'static str {
    match self {
      ParamDefault::IdentityFn => "function(x) x",
      ParamDefault::Null => "null",
      ParamDefault::True => "true",
      ParamDefault::False => "false",
      ParamDefault::NewlineChar => "\"\\n\"",
      ParamDefault::ColonSpace => "\": \"",
    }
  }
}

impl fmt::Display for ParamDefault {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.write_str(self.as_static_str())
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
  /// An set (sorted duplicate-free array) of strings, like `["a", "b"]`.
  SetStr,
  /// An array of `{ key: string, value: any }`.
  ArrKv,
  /// A set with any contents.
  SetAny,
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
  /// A function with 1 non-specifically-named param.
  Fn1,
  /// A function with 2 params: an accumulator and an array element. Used for folds.
  FnAccElem,
  /// A function with 2 params: a key and a value. Used for map with key.
  FnKv,
  /// A function with 2 params: an index and an array element. Used for map with index.
  FnIdxElem,
}

/// Short for "required".
const fn req(name: &'static str, ty: Ty) -> Param {
  Param { name, ty, default: None }
}

/// Short for "optional".
const fn opt(name: &'static str, ty: Ty, default: ParamDefault) -> Param {
  Param { name, ty, default: Some(default) }
}

/// Short for "signature".
const fn sig(params: &'static [Param], ret: Ty) -> Sig {
  Sig { params, ret }
}

/// Given `(func, res, ...args)`, returns a static string of a Jsonnet expression that evaluates to
/// whether the std `func` applied to `args` returns a number that is epsilon-equal (i.e. very
/// close) to `res`.
///
/// The args must be separated by commas and there must be at least one arg.
macro_rules! epsilon_eq {
  ($name:expr, $res:expr, $fst:expr $(, $rest:expr)* $(,)?) => {
    concat!("std.abs(std.", $name, "(", $fst, $(", ", $rest, )* ") - ", $res, ") <= 0.01")
  };
}

const KEY_F: Param = opt("keyF", Ty::Fn1, ParamDefault::IdentityFn);
const ON_EMPTY: Param = opt("onEmpty", Ty::Any, ParamDefault::Null);
const ARR_ANY: Param = req("arr", Ty::ArrAny);
const V_ANY_RET_BOOL: Sig = sig(&[req("v", Ty::Any)], Ty::Bool);
const X_NUM_RET_NUM: Sig = sig(&[req("x", Ty::Num)], Ty::Num);
const N_NUM_RET_NUM: Sig = sig(&[req("n", Ty::Num)], Ty::Num);
const X_NUM_RET_BOOL: Sig = sig(&[req("x", Ty::Num)], Ty::Bool);
const STR_RET_STR: Sig = sig(&[req("str", Ty::Str)], Ty::Str);
const X_Y_BOOL_RET_BOOL: Sig = sig(&[req("x", Ty::Bool), req("y", Ty::Bool)], Ty::Bool);
const A_B_STR_RET_BOOL: Sig = sig(&[req("a", Ty::Str), req("b", Ty::Str)], Ty::Bool);
const A_B_NUM: Sig = sig(&[req("a", Ty::Num), req("b", Ty::Num)], Ty::Num);
const STR_CHARS_STR_RET_STR: Sig = sig(&[req("str", Ty::Str), req("chars", Ty::Str)], Ty::Str);
const STR_RET_NUM: Sig = sig(&[req("str", Ty::Str)], Ty::Num);
const STR_RET_ANY: Sig = sig(&[req("str", Ty::Str)], Ty::Any);
const SPLIT_LIMIT: Sig =
  sig(&[req("str", Ty::Str), req("c", Ty::Str), req("maxsplits", Ty::Num)], Ty::ArrStr);
const OBJ_HAS: Sig = sig(&[req("o", Ty::Obj), req("f", Ty::StrInterned)], Ty::Bool);
const OBJ_FIELDS: Sig = sig(&[req("o", Ty::Obj)], Ty::SetStr);
const OBJ_VALUES: Sig = sig(&[req("o", Ty::Obj)], Ty::ArrAny);
const OBJ_KEYS_VALUES: Sig = sig(&[req("o", Ty::Obj)], Ty::ArrKv);
const MANIFEST_JSON: Sig = sig(&[req("value", Ty::Any)], Ty::Str);
const ARR_HOF1: Sig = sig(&[req("func", Ty::Fn1), req("arr", Ty::ArrAny)], Ty::ArrAny);
const FOLD: Sig =
  sig(&[req("func", Ty::FnAccElem), req("arr", Ty::ArrAny), req("init", Ty::Any)], Ty::Any);
const ARR_KEY_F: Sig = sig(&[req("arr", Ty::ArrAny), KEY_F], Ty::ArrAny);
const BINARY_SET_FN: Sig = sig(&[req("a", Ty::SetAny), req("b", Ty::SetAny), KEY_F], Ty::SetAny);
const HASH: Sig = sig(&[req("s", Ty::Str)], Ty::Str);

/// The std fns.
pub const FNS: [Fn; 147] = [
  Fn {
    name: S::new("native"),
    implemented: false,
    sig: sig(&[req("x", Ty::Str)], Ty::Any),
    total: true,
    available_since: None,
    doc: indoc! {"
      Returns a natively-implemented function with the given name.

      Some interpreters permit implementing functions in the host language and exposing them via
      this function.
    "},
    examples: Examples::EMPTY_ALLOWED,
  },
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
    examples: Examples::EMPTY_ALLOWED,
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
    examples: Examples::new(&[
      r#" std.type([1]) == "array" "#,
      r#" std.type(null) == "null" "#,
      r#" std.type({}) == "object" "#,
      r#" std.type(3) == "number" "#,
    ]),
  },
  Fn {
    name: S::new("isArray"),
    implemented: true,
    sig: V_ANY_RET_BOOL,
    total: true,
    available_since: None,
    doc: "Returns whether the argument is an array.",
    examples: Examples::new(&[
      "std.isArray([1, 2])",
      "std.isArray([])",
      "!std.isArray(null)",
      "!std.isArray(4)",
    ]),
  },
  Fn {
    name: S::new("isBoolean"),
    implemented: true,
    sig: V_ANY_RET_BOOL,
    total: true,
    available_since: None,
    doc: "Returns whether the argument is a boolean, i.e. `true` or `false`.",
    examples: Examples::new(&[
      "std.isBoolean(true)",
      "std.isBoolean(false)",
      "!std.isBoolean(null)",
      "!std.isBoolean(4)",
    ]),
  },
  Fn {
    name: S::new("isFunction"),
    implemented: true,
    sig: V_ANY_RET_BOOL,
    total: true,
    available_since: None,
    doc: "Returns whether the argument is a function.",
    examples: Examples::new(&[
      "std.isFunction(function(x) x + 1)",
      "std.isFunction(std.mod)",
      "!std.isFunction(null)",
      "!std.isFunction(4)",
    ]),
  },
  Fn {
    name: S::new("isNumber"),
    implemented: true,
    sig: V_ANY_RET_BOOL,
    total: true,
    available_since: None,
    doc: "Returns whether the argument is a number.",
    examples: Examples::new(&[
      "std.isNumber(3)",
      "std.isNumber(-123.345)",
      "!std.isNumber(null)",
      "!std.isNumber([])",
    ]),
  },
  Fn {
    name: S::new("isObject"),
    implemented: true,
    sig: V_ANY_RET_BOOL,
    total: true,
    available_since: None,
    doc: "Returns whether the argument is an object.",
    examples: Examples::new(&[
      "std.isObject({})",
      "std.isObject({ a: 1 } + { b: 2 })",
      "!std.isObject(null)",
      "!std.isObject([])",
    ]),
  },
  Fn {
    name: S::new("isString"),
    implemented: true,
    sig: V_ANY_RET_BOOL,
    total: true,
    available_since: None,
    doc: "Returns whether the argument is a string.",
    examples: Examples::new(&[
      r#" std.isString("hi") "#,
      r#" std.isString("") "#,
      r#" !std.isString(null) "#,
      r#" !std.isString({}) "#,
    ]),
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

      | Type     | Something              |
      | -------- | ---------------------- |
      | array    | elements               |
      | string   | codepoints (NOT bytes) |
      | function | required parameters    |
      | object   | non-hidden fields      |

      Raises an error if given `null`, `true`, `false`, or a number.
    "#},
    examples: Examples::new(&[
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
      r#" std.length({ x:: 9, y::: 7 }) == 1 "#,
    ]),
  },
  Fn {
    name: S::new("get"),
    implemented: true,
    sig: sig(
      &[
        req("o", Ty::Obj),
        req("f", Ty::StrInterned),
        opt("default", Ty::Any, ParamDefault::Null),
        opt("inc_hidden", Ty::Bool, ParamDefault::True),
      ],
      Ty::Any,
    ),
    total: true,
    available_since: Some(18),
    doc: indoc! {"
      Returns the object `o`'s field `f` if it exists or `default` value otherwise. `inc_hidden`
      controls whether to include hidden fields.
    "},
    examples: Examples::new(&[
      r#" std.get({ hi: 4 }, "hi", 3) == 4 "#,
      r#" std.get({}, "hi", 3) == 3 "#,
      r#" std.get({ hi:: 5 }, "hi", 3) == 5 "#,
      r#" std.get({ hi:: 5 }, "hi", 3, false) == 3 "#,
    ]),
  },
  Fn {
    name: S::new("objectHas"),
    implemented: true,
    sig: OBJ_HAS,
    total: true,
    available_since: Some(10),
    doc: indoc! {"
      Returns whether the given object `o` has the field `f`.

      Returns `false` if the field is hidden.
    "},
    examples: Examples::new(&[
      r#" !std.objectHas({}, "hi") "#,
      r#" std.objectHas({ hi: 3 }, "hi") "#,
      r#" !std.objectHas({ hi:: 3 }, "hi") "#,
    ]),
  },
  Fn {
    name: S::new("objectFields"),
    implemented: true,
    sig: OBJ_FIELDS,
    total: true,
    available_since: Some(10),
    doc: indoc! {"
      Returns an array of strings, each element being a field from the given object.

      **Does not** include hidden fields.
    "},
    examples: Examples::new(&[
      r#" std.objectFields({}) == [] "#,
      r#" std.objectFields({ a: 1, b: 2 }) == ["a", "b"] "#,
      r#" std.objectFields({ a:: 1, b: 2 }) == ["b"] "#,
    ]),
  },
  Fn {
    name: S::new("objectValues"),
    implemented: true,
    sig: OBJ_VALUES,
    total: true,
    available_since: Some(17),
    doc: indoc! {"
      Returns an array of the values in the given object.

      **Does not** include hidden fields.
    "},
    examples: Examples::new(&[
      r#" std.objectValues({}) == [] "#,
      r#" std.objectValues({ a: 1, b: 2 }) == [1, 2] "#,
      r#" std.objectValues({ a:: 1, b: 2 }) == [2] "#,
    ]),
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
    examples: Examples::new(&[
      r#" std.objectKeysValues({}) == [] "#,
      r#" std.objectKeysValues({ a: 1, b: 2 }) == [{ key: "a", value: 1 }, { key: "b", value: 2 }] "#,
      r#" std.objectKeysValues({ a:: 1, b: 2 }) == [{ key: "b", value: 2 }] "#,
    ]),
  },
  Fn {
    name: S::new("objectHasAll"),
    implemented: true,
    sig: OBJ_HAS,
    total: true,
    available_since: Some(10),
    doc: indoc! {"
      Like `std.objectHas` but also includes hidden fields.
    "},
    examples: Examples::new(&[
      r#" !std.objectHasAll({}, "hi") "#,
      r#" std.objectHasAll({ hi: 3 }, "hi") "#,
      r#" std.objectHasAll({ hi:: 3 }, "hi") "#,
    ]),
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
    examples: Examples::new(&[
      r#" std.objectFieldsAll({}) == [] "#,
      r#" std.objectFieldsAll({ a: 1, b: 2 }) == ["a", "b"] "#,
      r#" std.objectFieldsAll({ a:: 1, b: 2 }) == ["a", "b"] "#,
    ]),
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
    examples: Examples::new(&[
      r#" std.objectValuesAll({}) == [] "#,
      r#" std.objectValuesAll({ a: 1, b: 2 }) == [1, 2] "#,
      r#" std.objectValuesAll({ a:: 1, b: 2 }) == [1, 2] "#,
    ]),
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
    examples: Examples::new(&[
      r#" std.objectKeysValuesAll({}) == [] "#,
      r#" std.objectKeysValuesAll({ a: 1, b: 2 }) == [{ key: "a", value: 1 }, { key: "b", value: 2 }] "#,
      r#" std.objectKeysValuesAll({ a:: 1, b: 2 }) == [{ key: "a", value: 1 }, { key: "b", value: 2 }] "#,
    ]),
  },
  Fn {
    name: S::new("objectRemoveKey"),
    implemented: false,
    sig: sig(&[req("obj", Ty::Obj), req("key", Ty::Str)], Ty::Obj),
    total: true,
    available_since: Some(21),
    doc: indoc! {"
      Returns the given object after removing the given key, if it was present.
    "},
    examples: Examples::new(&[
      r#" std.objectRemoveKey({a: 2, b: 4}, "a") == {b: 4} "#,
      r#" std.objectRemoveKey({a: 3}, "b") == {a: 3} "#,
    ]),
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
    examples: Examples::new(&[
      r#" std.prune([1, [], 2, {}, 3, null]) == [1, 2, 3] "#,
      r#" std.prune({ a: 3 }) == {a: 3} "#,
      r#" std.prune({ w: 0, x: "", y: [], z: null }) == {w: 0, x: ""} "#,
      r#" std.prune(null) == null "#,
    ]),
  },
  Fn {
    name: S::new("mapWithKey"),
    implemented: false,
    sig: sig(&[req("func", Ty::FnKv), req("obj", Ty::Obj)], Ty::Obj),
    total: true,
    available_since: Some(10),
    doc: indoc! {"
      Applies the given `func` to all fields of the given `obj`, also passing the field name.

      `func` is expected to take the field name as the first parameter and the field value as the
      second.
    "},
    examples: Examples::new(&[
      indoc! {r#"
        std.mapWithKey(function(a, b) a + b, { a: 1, b: 2 }) == {
          a: "a1",
          b: "b2",
        }
      "#},
      r#" std.mapWithKey(function(a, b) a + b, { a:: 1, b: 2 }) == { b: "b2" } "#,
    ]),
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
    examples: Examples::new(&["std.abs(3) == 3", "std.abs(-1.2) == 1.2", "std.abs(0) == 0"]),
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
    examples: Examples::new(&["std.sign(3) == 1", "std.sign(-1.2) == -1", "std.sign(0) == 0"]),
  },
  Fn {
    name: S::new("max"),
    implemented: true,
    sig: A_B_NUM,
    total: true,
    available_since: None,
    doc: indoc! {"
      Returns the maximum of the two arguments.
    "},
    examples: Examples::new(&["std.max(3, 2) == 3", "std.max(4, 4) == 4", "std.max(-5, 1) == 1"]),
  },
  Fn {
    name: S::new("min"),
    implemented: true,
    sig: A_B_NUM,
    total: true,
    available_since: None,
    doc: indoc! {"
      Returns the minimum of the two arguments.
    "},
    examples: Examples::new(&["std.min(3, 2) == 2", "std.min(4, 4) == 4", "std.min(-5, 1) == -5"]),
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
    examples: Examples::new(&[
      "std.pow(2, 3) == 8",
      "std.pow(3, 2) == 9",
      "std.pow(1, 99) == 1",
      "std.pow(0, 2) == 0",
      "std.pow(99, 0) == 1",
    ]),
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
    examples: Examples::new(&[
      "std.exp(0) == 1",
      epsilon_eq!("exp", 2.71, 1),
      epsilon_eq!("exp", 7.38, 2),
    ]),
  },
  Fn {
    name: S::new("log"),
    implemented: true,
    sig: X_NUM_RET_NUM,
    total: true,
    available_since: None,
    doc: indoc! {r"
      Returns the natural logarithm of $x$, i.e. the solution $y$ in $e^y = x$, where
      [$e \approx 2.71828$](<https://en.wikipedia.org/wiki/E_(mathematical_constant)>).
    "},
    examples: Examples::new(&[
      "std.log(1) == 0",
      epsilon_eq!("log", 4.81, 123),
      epsilon_eq!("log", 5.84, 345),
    ]),
  },
  Fn {
    name: S::new("log2"),
    implemented: false,
    sig: X_NUM_RET_NUM,
    total: true,
    available_since: None,
    doc: indoc! {r"
      Returns the base-2 logarithm of $x$, i.e. the solution $y$ in $2^y = x$.
    "},
    examples: Examples::new(&["std.log2(1) == 0", "std.log2(8) == 3"]),
  },
  Fn {
    name: S::new("log10"),
    implemented: false,
    sig: X_NUM_RET_NUM,
    total: true,
    available_since: None,
    doc: indoc! {r"
      Returns the base-10 logarithm of $x$, i.e. the solution $y$ in $10^y = x$.
    "},
    examples: Examples::new(&["std.log10(1) == 0", "std.log10(100) == 2"]),
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
    examples: Examples::new(&[
      "std.exponent(0) == 0",
      "std.exponent(4) == 3",
      "std.exponent(123456789) == 27",
      "std.exponent(-789456) == 20",
    ]),
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
    examples: Examples::new(&[
      "std.mantissa(0) == 0",
      "std.mantissa(4) == 0.5",
      epsilon_eq!("mantissa", 0.91, 123456789),
      epsilon_eq!("mantissa", "-0.75", "-789456"),
    ]),
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
    examples: Examples::new(&[
      "std.floor(1) == 1",
      "std.floor(1.99) == 1",
      "std.floor(2.01) == 2",
      "std.floor(-1) == -1",
      "std.floor(-1.01) == -2",
      "std.floor(-1.99) == -2",
      "std.floor(-2.01) == -3",
    ]),
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
    examples: Examples::new(&[
      "std.ceil(1) == 1",
      "std.ceil(1.99) == 2",
      "std.ceil(2.01) == 3",
      "std.ceil(-1) == -1",
      "std.ceil(-1.01) == -1",
      "std.ceil(-1.99) == -1",
      "std.ceil(-2.01) == -2",
    ]),
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
    examples: Examples::new(&["std.sqrt(9) == 3", "std.sqrt(4) == 2", "std.sqrt(1) == 1"]),
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
    examples: Examples::new(&["std.sin(0) == 0", epsilon_eq!("sin", 0.47, 0.5)]),
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
    examples: Examples::new(&["std.cos(0) == 1", epsilon_eq!("cos", 0.87, 0.5)]),
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
    examples: Examples::new(&["std.tan(0) == 0", epsilon_eq!("tan", 0.54, 0.5)]),
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
    examples: Examples::new(&["std.asin(0) == 0", epsilon_eq!("asin", 0.52, 0.5)]),
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
    examples: Examples::new(&[epsilon_eq!("acos", 1.57, 0), epsilon_eq!("acos", 1.04, 0.5)]),
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
    examples: Examples::new(&["std.atan(0) == 0", epsilon_eq!("atan", 0.46, 0.5)]),
  },
  Fn {
    name: S::new("atan2"),
    implemented: false,
    sig: sig(&[req("y", Ty::Num), req("x", Ty::Num)], Ty::Num),
    total: true,
    available_since: None,
    doc: indoc! {"
      Returns the argument (also called phase or angle) of the complex number $x + iy$.
    "},
    examples: Examples::new(&[
      epsilon_eq!("atan2", 0, 0, 1),
      epsilon_eq!("atan2", "std.pi / 4", 1, 1),
      epsilon_eq!("atan2", "-std.pi / 4", "-1", 1),
      epsilon_eq!("atan2", "std.pi", 0, "-1"),
      epsilon_eq!("atan2", 2.83, 1.2, "-3.8"),
    ]),
  },
  Fn {
    name: S::new("hypot"),
    implemented: false,
    sig: A_B_NUM,
    total: true,
    available_since: None,
    doc: indoc! {"
      Returns the square root of the sum of the squares of x and y.
    "},
    examples: Examples::new(&[
      "std.hypot(3, 4) == 5",
      "std.hypot(5, 12) == 13",
      "std.hypot(8, 15) == 17",
      "std.hypot(7, 24) == 25",
    ]),
  },
  Fn {
    name: S::new("deg2rad"),
    implemented: false,
    sig: X_NUM_RET_NUM,
    total: true,
    available_since: None,
    doc: indoc! {"
      Converts the argument from degrees to radians.
    "},
    examples: Examples::new(&[
      "std.deg2rad(0) == 0",
      epsilon_eq!("deg2rad", "std.pi / 4", 45),
      epsilon_eq!("deg2rad", "std.pi / 2", 90),
      epsilon_eq!("deg2rad", 3, 172),
    ]),
  },
  Fn {
    name: S::new("rad2deg"),
    implemented: false,
    sig: X_NUM_RET_NUM,
    total: true,
    available_since: None,
    doc: indoc! {"
      Converts the argument from radians to degrees.
    "},
    examples: Examples::new(&[
      "std.rad2deg(0) == 0",
      epsilon_eq!("rad2deg", 45, "std.pi / 4",),
      epsilon_eq!("rad2deg", 90, "std.pi / 2",),
      epsilon_eq!("rad2deg", 172, 3,),
    ]),
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
    examples: Examples::new(&[
      "std.round(1) == 1",
      "std.round(1.1) == 1",
      "std.round(1.5) == 2",
      "std.round(1.9) == 2",
      "std.round(-1) == -1",
      "std.round(-1.1) == -1",
      "std.round(-1.5) == -2",
      "std.round(-1.9) == -2",
    ]),
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
    examples: Examples::new(&[
      "std.isEven(2)",
      "std.isEven(0)",
      "std.isEven(-2)",
      "!std.isEven(1)",
      "!std.isEven(9)",
      "!std.isEven(-5)",
      "!std.isEven(4.4)",
      "!std.isEven(5.5)",
    ]),
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
    examples: Examples::new(&[
      "std.isOdd(1)",
      "std.isOdd(9)",
      "std.isOdd(-5)",
      "!std.isOdd(2)",
      "!std.isOdd(0)",
      "!std.isOdd(-2)",
      "!std.isOdd(4.4)",
      "!std.isOdd(5.5)",
    ]),
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
    examples: Examples::new(&[
      "std.isInteger(1)",
      "std.isInteger(0)",
      "std.isInteger(-5)",
      "!std.isInteger(0.1)",
      "!std.isInteger(4.4)",
      "!std.isInteger(2.0001)",
    ]),
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
    examples: Examples::new(&[
      "std.isDecimal(0.1)",
      "std.isDecimal(4.4)",
      "std.isDecimal(2.0001)",
      "!std.isDecimal(1)",
      "!std.isDecimal(0)",
      "!std.isDecimal(-5)",
    ]),
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
    examples: Examples::EMPTY_ALLOWED,
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
    examples: Examples::new(&[
      "std.clamp(-3, 0, 5) == 0",
      "std.clamp(4, 0, 5) == 4",
      "std.clamp(7, 0, 5) == 5",
    ]),
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
    examples: Examples::new(&["std.assertEqual(2 + 2, 4)", "std.assertEqual(12 - 34, -22))"]),
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
    examples: Examples::new(&[
      r#" std.toString("a") == "a" "#,
      r#" std.toString(123) == "123" "#,
      r#" std.toString(null) == "null" "#,
      r#" std.toString([2, 5]) == "[2, 5]" "#,
      r#" std.toString({a: 1}) == "{\"a\": 1}" "#,
    ]),
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
    examples: Examples::new(&[
      r#" std.codepoint("A") == 65 "#,
      r#" std.codepoint("a") == 97 "#,
      r#" std.codepoint("あ") == 12354 "#,
    ]),
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
    examples: Examples::new(&[
      r#" std.char(65) == "A" "#,
      r#" std.char(97) == "a" "#,
      r#" std.char(12354) == "あ" "#,
    ]),
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
    examples: Examples::new(&[
      r#" std.substr("think", 1, 2) == "hi" "#,
      r#" std.substr("develop", 4, 3) == "lop" "#,
      r#" std.substr("hello world", 6, 99) == "world" "#,
    ]),
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
    examples: Examples::new(&[
      r#" std.findSubstr("e", "envelope") == [0, 3, 7] "#,
      r#" std.findSubstr("hi", "hi Chidi") == [0, 4] "#,
      r#" std.findSubstr("fork", "shirt") == [] "#,
    ]),
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
    examples: Examples::new(&[
      r#" std.startsWith("hi Chidi", "hi") "#,
      r#" !std.startsWith("hi Chidi", "fork") "#,
    ]),
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
    examples: Examples::new(&[
      r#" std.endsWith("thank you", "you") "#,
      r#" !std.endsWith("thank you", "no") "#,
    ]),
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
    examples: Examples::new(&[
      r#" std.stripChars(" test test test ", " ") == "test test test" "#,
      r#" std.stripChars("aaabbbbcccc", "ac") == "bbbb" "#,
      r#" std.stripChars("cacabbbbaacc", "ac") == "bbbb" "#,
    ]),
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
    examples: Examples::new(&[
      r#" std.lstripChars(" test test test ", " ") == "test test test " "#,
      r#" std.lstripChars("aaabbbbcccc", "ac") == "bbbbcccc" "#,
      r#" std.lstripChars("cacabbbbaacc", "ac") == "bbbbaacc" "#,
    ]),
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
    examples: Examples::new(&[
      r#" std.rstripChars(" test test test ", " ") == " test test test" "#,
      r#" std.rstripChars("aaabbbbcccc", "ac") == "aaabbbb" "#,
      r#" std.rstripChars("cacabbbbaacc", "ac") == "cacabbbb" "#,
    ]),
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
    examples: Examples::new(&[
      r#" std.split("foo/bar", "/") == ["foo", "bar"] "#,
      r#" std.split("/foo/bar", "/") == ["", "foo", "bar"] "#,
    ]),
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
    examples: Examples::new(&[
      r#" std.splitLimit("foo/bar", "/", 1) == ["foo", "bar"] "#,
      r#" std.splitLimit("/foo/bar", "/", 1) == ["", "foo/bar"] "#,
    ]),
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
    examples: Examples::new(&[
      r#" std.splitLimitR("/foo/bar", "/", 1) == ["/foo", "bar"] "#,
      r#" std.splitLimitR("/foo/bar", "/", 2) == ["", "foo", "bar"] "#,
    ]),
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
    examples: Examples::new(&[
      r#"  std.strReplace("pet the dog", "dog", "cat") == "pet the cat" "#,
      r#"  std.strReplace("pet the dog", "fish", "unicorn") == "pet the dog" "#,
      indoc! {r#"
        std.strReplace("I like to skate with my skateboard", "skate", "surf")
          == "I like to surf with my surfboard"
      "#},
    ]),
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
    examples: Examples::new(&[
      r#" std.isEmpty("") "#,
      r#" !std.isEmpty("hi") "#,
      r#" !std.isEmpty("hello world") "#,
    ]),
  },
  Fn {
    name: S::new("trim"),
    implemented: false,
    sig: sig(&[req("str", Ty::Str)], Ty::Str),
    total: true,
    available_since: Some(21),
    doc: indoc! {"
      Returns the string with leading and trailing whitespaces removed.
    "},
    examples: Examples::new(&[
      r#" std.trim("hi") == "hi" "#,
      r#" std.trim("hello\t") == "hello" "#,
      r#" std.trim("  hey\n\n") == "hey" "#,
      r#" std.trim(" hello world  ") == "hello world" "#,
    ]),
  },
  Fn {
    name: S::new("asciiUpper"),
    implemented: true,
    sig: STR_RET_STR,
    total: true,
    available_since: Some(10),
    doc: indoc! {r#"
      Returns a copy of the string in which all ASCII letters are capitalized.

      Non-ASCII letters, even those which may have a notion of upper or lower case, will not be
      changed.
    "#},
    examples: Examples::new(&[
      r#" std.asciiUpper("100 Cats!") == "100 CATS!" "#,
      r#" std.asciiUpper("César") == "CéSAR" "#,
    ]),
  },
  Fn {
    name: S::new("asciiLower"),
    implemented: true,
    sig: STR_RET_STR,
    total: true,
    available_since: Some(10),
    doc: indoc! {r#"
      Returns a copy of the string in which all ASCII letters are lower cased.

      Non-ASCII letters, even those which may have a notion of upper or lower case, will not be
      changed.
    "#},
    examples: Examples::new(&[
      r#" std.asciiLower("100 Cats!") == "100 cats!" "#,
      r#" std.asciiLower("CÉSAR") == "cÉsar" "#,
    ]),
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
    examples: Examples::new(&[
      r#" std.stringChars("foo") == ["f", "o", "o"] "#,
      r#" std.stringChars("はい") == ["は", "い"]"#,
      r#" std.stringChars("") == []"#,
    ]),
  },
  Fn {
    name: S::new("format"),
    implemented: true,
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
    examples: Examples::new(&[
      r#" std.format("Hello %03d", 12) == "Hello 012" "#,
      r#" "Hello %03d" % 12 == "Hello 012" "#,
      r#" "Hello %s, age %d" % ["Foo", 25] == "Hello Foo, age 25" "#,
      indoc! {r#"
      "Hello %(name)s, age %(age)d" % {age: 25, name: "Foo"}
        == "Hello Foo, age 25"
      "#},
    ]),
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
    examples: Examples::new(&[
      r#" std.escapeStringBash("echo 'hi'") == "'echo '\"'\"'hi'\"'\"''" "#,
      r#" std.escapeStringBash("ls -l") == "'ls -l'" "#,
    ]),
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
    examples: Examples::new(&[
      r#" std.escapeStringDollars("1 + $x") == "1 + $$x" "#,
      r#" std.escapeStringDollars("hi there") == "hi there" "#,
    ]),
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
    examples: Examples::new(&[
      indoc! {r#"
        "{name: %s}" % std.escapeStringJson("Multiline\nc:\\path")
          == "{name: \"Multiline\\nc:\\\\path\"}"
      "#},
      r#" std.escapeStringJson("hi") == '"hi"' "#,
    ]),
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
    examples: Examples::new(&[
      indoc! {r#"
        "{name: %s}" % std.escapeStringPython("Multiline\nc:\\path")
          == "{name: \"Multiline\\nc:\\\\path\"}"
      "#},
      r#" std.escapeStringPython("hi") == '"hi"' "#,
    ]),
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
    examples: Examples::new(&[
      r#" std.escapeStringXml("2 > 1") == "2 &gt; 1" "#,
      r#" std.escapeStringXml("that's great") == "that&apos;s great" "#,
    ]),
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
    examples: Examples::new(&[
      r#" std.parseInt("123") == 123 "#,
      r#" std.parseInt("-123") == -123 "#,
    ]),
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
    examples: Examples::new(&[
      r#" std.parseOctal("755") == 493 "#,
      r#" std.parseOctal("0755") == 493 "#,
    ]),
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
    examples: Examples::new(&[
      r#" std.parseHex("ff") == 255 "#,
      r#" std.parseHex("BADc0de") == 195936478 "#,
    ]),
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
    examples: Examples::new(&[
      r#" std.parseJson("null") == null "#,
      r#" std.parseJson("[2, false]") == [2, false] "#,
      r#" std.parseJson('{"foo": "bar"}') == { "foo": "bar" } "#,
    ]),
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

      YAML is a superset of JSON, consequently "down-casting" or manifestation of YAML into JSON or
      Jsonnet values will only succeed when using the subset of YAML that is compatible with JSON.

      The parser does not support YAML documents with scalar values at the root. The root node of a
      YAML document must start with either a YAML sequence or map to be successfully parsed.
    "#},
    examples: Examples::new(&[
      r#" std.parseYaml("foo: bar") == { "foo": "bar" } "#,
      r#" std.parseYaml([1, 2]) == [1, 2] "#,
    ]),
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
    examples: Examples::new(&[
      r#" std.encodeUTF8("hey") == [104, 101, 121] "#,
      r#" std.encodeUTF8("あ") == [227, 129, 130] "#,
    ]),
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
    examples: Examples::new(&[
      r#" std.decodeUTF8([104, 101, 121]) == "hey" "#,
      r#" std.decodeUTF8([227, 129, 130]) == "あ" "#,
    ]),
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
    examples: Examples::EMPTY_ALLOWED,
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
    examples: Examples::EMPTY_ALLOWED,
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
    examples: Examples::EMPTY_ALLOWED,
  },
  Fn {
    name: S::new("manifestJsonEx"),
    implemented: false,
    sig: sig(
      &[
        req("value", Ty::Any),
        req("indent", Ty::Str),
        opt("newline", Ty::Str, ParamDefault::NewlineChar),
        opt("key_val_sep", Ty::Str, ParamDefault::ColonSpace),
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
    examples: Examples::EMPTY_TODO,
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
    examples: Examples::EMPTY_TODO,
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
    examples: Examples::EMPTY_TODO,
  },
  Fn {
    name: S::new("manifestYamlDoc"),
    implemented: false,
    sig: sig(
      &[
        req("value", Ty::Any),
        opt("indent_array_in_object", Ty::Bool, ParamDefault::False),
        opt("quote_keys", Ty::Bool, ParamDefault::True),
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
    examples: Examples::EMPTY_ALLOWED,
  },
  Fn {
    name: S::new("manifestYamlStream"),
    implemented: false,
    sig: sig(
      &[
        req("value", Ty::ArrAny),
        opt("indent_array_in_object", Ty::Bool, ParamDefault::False),
        opt("c_document_end", Ty::Bool, ParamDefault::False),
        opt("quote_keys", Ty::Bool, ParamDefault::True),
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
    examples: Examples::EMPTY_ALLOWED,
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
    examples: Examples::EMPTY_ALLOWED,
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
    examples: Examples::EMPTY_TODO,
  },
  Fn {
    name: S::new("makeArray"),
    implemented: true,
    sig: sig(&[req("sz", Ty::Uint), req("func", Ty::Fn1)], Ty::ArrAny),
    total: true,
    available_since: Some(10),
    doc: indoc! {"
      Creates a new array of `sz` elements by calling `func` to initialize each element.

      `func` is a function that takes a single parameter, the index of the element it should
      initialize.
    "},
    examples: Examples::new(&[
      r#" std.makeArray(3, function(x) x * x) == [0, 1, 4] "#,
      r#" std.makeArray(0, function(x) error "called on %d" % x) == [] "#,
    ]),
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
    examples: Examples::new(&[
      r#" std.member([1, 2], 2) "#,
      r#" !std.member([1, 2], 3) "#,
      r#" std.member("hello", "l") "#,
      r#" !std.member("hello", "z") "#,
    ]),
  },
  Fn {
    name: S::new("count"),
    implemented: false,
    sig: sig(&[ARR_ANY, req("x", Ty::Any)], Ty::Uint),
    total: true,
    available_since: Some(10),
    doc: indoc! {"
      `std.count(arr, x)` returns the number of times that `x` occurs in `arr`.
    "},
    examples: Examples::new(&[
      r#" std.count([1, 2], 2) == 1 "#,
      r#" std.count([1, 2], 3) == 0 "#,
      r#" std.count([3, 1, 3, 0], 3) == 2 "#,
    ]),
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
    examples: Examples::new(&[
      r#" std.find(2, [1, 2]) == [0] "#,
      r#" std.find(3, [1, 2]) == [] "#,
      r#" std.find(3, [3, 1, 3, 0]) == [0, 2] "#,
    ]),
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
    examples: Examples::new(&[
      r#" std.map(function(x) x + 1, [2, 4]) == [3, 5] "#,
      r#" std.map(function(x) error "oh no", []) == [] "#,
    ]),
  },
  Fn {
    name: S::new("mapWithIndex"),
    implemented: false,
    sig: sig(&[req("func", Ty::FnIdxElem), req("arr", Ty::ArrAny)], Ty::ArrAny),
    total: true,
    available_since: Some(10),
    doc: indoc! {"
      Similar to `std.map`, but it also passes to the function the element's index in the array.

      The function is expected to take the index as the first parameter and the element as the
      second.
    "},
    examples: Examples::new(&[
      r#" std.mapWithIndex(function(i, x) x + i, [2, 4]) == [2, 5] "#,
      r#" std.mapWithIndex(function(i, x) error "oh no", []) == [] "#,
    ]),
  },
  Fn {
    name: S::new("filterMap"),
    implemented: false,
    sig: sig(
      &[req("filter_func", Ty::Fn1), req("map_func", Ty::Fn1), req("arr", Ty::ArrAny)],
      Ty::ArrAny,
    ),
    total: true,
    available_since: Some(10),
    doc: indoc! {"
      First filters with `filter_func`, then maps with `map_func`, the given array `arr`.
    "},
    examples: Examples::new(&[
      indoc! {"
        std.filterMap(
          function(x) x > 3,
          function(x) x + 2,
          [1, 6, 2, 5],
        ) == [
          8,
          7,
        ]
      "},
      indoc! {r#"
        std.filterMap(
          function(x) false,
          function(x) error "oh no",
          [1, 6, 2, 5],
        ) == []
      "#},
    ]),
  },
  Fn {
    name: S::new("flatMap"),
    implemented: false,
    sig: sig(&[req("func", Ty::Fn1), req("arr", Ty::StrOrArrAny)], Ty::StrOrArrAny),
    total: true,
    available_since: Some(10),
    doc: indoc! {r#"
      Applies the given function to every element of `arr`, then flattens the result.

      The argument `arr` must be an array or a string.

      - If `arr` is an array, `func` must return an array.
      - If `arr` is a string, `func` must return a string.

      Can be thought of as a generalized `map`, with each element mapped to 0, 1 or more elements.
    "#},
    examples: Examples::new(&[
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
        std.flatMap(function(x) x + x, "a&b")
          == "aa&&bb"
      "#},
    ]),
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
    examples: Examples::new(&[
      " std.filter(function(x) x > 3, [1, 6, 2, 8, 3, 8]) == [6, 8, 8] ",
      " std.filter(function(x) false, [1, 6, 2, 8, 3, 8]) == [] ",
    ]),
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
    examples: Examples::new(&[
      indoc! {r#"
        std.foldl(function(ac, x) "(%s %s)" % [ac, x], ["a", "b", "c"], "_")
          == "(((_ a) b) c)"
      "#},
      r" std.foldl(function(ac, x) ac + x, [1, 2, 3], 0) == 6 ",
    ]),
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
    examples: Examples::new(&[
      indoc! {r#"
        std.foldr(function(ac, x) "(%s %s)" % [ac, x], ["a", "b", "c"], "_")
          == "(((_ c) b) a)"
      "#},
      r" std.foldr(function(ac, x) ac + x, [1, 2, 3], 0) == 6 ",
    ]),
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
    examples: Examples::new(&[
      r#" std.range(2, 6) == [2, 3, 4, 5, 6] "#,
      r#" std.range(-1, 3) == [-1, 0, 1, 2, 3] "#,
      r#" std.range(0, 0) == [0] "#,
    ]),
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
    examples: Examples::new(&[
      r#" std.repeat([1, 2], 3) == [1, 2, 1, 2, 1, 2] "#,
      r#" std.repeat("boo", 2) == "booboo" "#,
    ]),
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
    examples: Examples::new(&[
      r#" std.slice([1, 2, 3, 4, 5, 6], 0, 4, 1) == [1, 2, 3, 4] "#,
      r#" std.slice([1, 2, 3, 4, 5, 6], 1, 6, 2) == [2, 4, 6] "#,
      r#" std.slice("jsonnet", 0, 4, 1) == "json" "#,
      r#" std.slice("jsonnet", -3, null, null) == "net" "#,
    ]),
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
    examples: Examples::new(&[
      r#" std.join(".", ["www", "google", "com"]) == "www.google.com" "#,
      r#" std.join([9, 9], [[1], [2, 3]]) == [ 1, 9, 9, 2, 3 ] "#,
    ]),
  },
  Fn {
    name: S::new("deepJoin"),
    implemented: false,
    sig: sig(&[req("arr", Ty::StrOrArrAny)], Ty::Str),
    total: true,
    available_since: Some(10),
    doc: indoc! {"
      Concatenates an array containing strings and arrays to form a single string.

      If `arr` is a string, it is returned unchanged.

      If `arr` is an array, it is flattened and the string elements are concatenated together with
      no separator.
    "},
    examples: Examples::new(&[
      indoc! {r#"
        std.deepJoin(["one ", ["two ", "three ", ["four "], []], "five ", ["six"]])
          == "one two three four five six"
      "#},
      r#" std.deepJoin("hello") == "hello" "#,
    ]),
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
    examples: Examples::new(&[
      indoc! {r#"
        std.lines([
          "cd /tmp",
          "ls -l",
          "mkdir -p foo",
        ]) == "cd /tmp\nls -l\nmkdir -p foo\n"
      "#},
      r#" std.lines([]) == "" "#,
    ]),
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
    examples: Examples::new(&[
      indoc! {"
        std.flattenArrays([[1, 2], [3], [[4], [5, 6]]])
          == [1, 2, 3, [4], [5, 6]]
      "},
      "std.flattenArrays([]) == []",
      "std.flattenArrays([[]]) == []",
    ]),
  },
  Fn {
    name: S::new("flattenDeepArray"),
    implemented: false,
    sig: sig(&[req("value", Ty::ArrAny)], Ty::ArrAny),
    total: true,
    available_since: Some(21),
    doc: indoc! {"
      Concatenates an array containing values and arrays into a single flattened array.
    "},
    examples: Examples::new(&[
      r#" std.flattenDeepArray([2, [4, [6]]]) == [2, 4, 6] "#,
      indoc! {r#"
        std.flattenDeepArray([[1, 2], [], [3, [4]], [[5, 6, [null]], [7, 8]]])
          == [1, 2, 3, 4, 5, 6, null, 7, 8]
      "#},
    ]),
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
    examples: Examples::new(&[
      "std.reverse([2, 4, 6]) == [6, 4, 2]",
      "std.reverse([8]) == [8]",
      "std.reverse([]) == []",
    ]),
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
    examples: Examples::EMPTY_ALLOWED,
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
    examples: Examples::new(&[
      "std.uniq([1, 1, 1]) == [1]",
      "std.uniq([1, 2, 2, 3, 2]) == [1, 2, 3, 2])",
    ]),
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
    examples: Examples::new(&[
      "std.all([true, true])",
      "std.all([])",
      "!std.all([true, false])",
      "!std.all([true, true, true, false, true])",
    ]),
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
    examples: Examples::new(&[
      "std.any([true, false])",
      "std.any([false, false, false, true, false])",
      "!std.any([false, false])",
      "!std.any([])",
    ]),
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
    examples: Examples::new(&["std.sum([]) == 0", "std.sum([1, 2, 3, 4]) == 10"]),
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
    examples: Examples::new(&[
      "std.avg([1, 3]) == 2",
      "std.avg([2, 4, 6, 5, 6, 7]) == 5",
      "std.avg([1, 2]) == 1.5",
    ]),
  },
  Fn {
    name: S::new("minArray"),
    implemented: false,
    sig: sig(&[ARR_ANY, KEY_F, ON_EMPTY], Ty::Any),
    total: true,
    available_since: Some(21),
    doc: indoc! {"
      Returns the minimum of all elements in `arr`.

      If `keyF` is provided, it is called on each element of the array and should return a
      comparator value, and in this case `maxArray` will return an element with the minimum
      comparator value.

      If `onEmpty` is provided, and `arr` is empty, then `maxArray` will return the provided
      `onEmpty` value.

      If `onEmpty` is not provided, then an empty `arr` will raise an error.
    "},
    examples: Examples::new(&[
      r#" std.minArray([1, 4, 3]) == 1 "#,
      r#" std.minArray([], onEmpty=2) == 2 "#,
    ]),
  },
  Fn {
    name: S::new("maxArray"),
    implemented: false,
    sig: sig(&[ARR_ANY, KEY_F, ON_EMPTY], Ty::Any),
    total: true,
    available_since: Some(21),
    doc: indoc! {"
      Returns the maximum of all elements in `arr`.

      If `keyF` is provided, it is called on each element of the array and should return a
      comparator value, and in this case `maxArray` will return an element with the maximum
      comparator value.

      If `onEmpty` is provided, and `arr` is empty, then `maxArray` will return the provided
      `onEmpty` value.

      If `onEmpty` is not provided, then an empty `arr` will raise an error.
    "},
    examples: Examples::new(&[
      r#" std.maxArray([1, 4, 3]) == 4 "#,
      r#" std.maxArray([], onEmpty=2) == 2 "#,
    ]),
  },
  Fn {
    name: S::new("contains"),
    implemented: false,
    sig: sig(&[req("arr", Ty::ArrAny), req("elem", Ty::Any)], Ty::Bool),
    total: true,
    available_since: Some(21),
    doc: indoc! {"
      Returns whether `arr` contains `elem`.
    "},
    examples: Examples::new(&[
      r#" std.contains([7, 1, 5], 1) "#,
      r#" std.contains([2, 3, 2, 4], 2) "#,
      r#" !std.contains([4, 6], 5) "#,
      r#" !std.contains([], 3) "#,
    ]),
  },
  Fn {
    name: S::new("remove"),
    implemented: false,
    sig: sig(&[ARR_ANY, req("elem", Ty::Any)], Ty::ArrAny),
    total: true,
    available_since: Some(21),
    doc: indoc! {"
      Removes the first occurrence of `elem` from `arr`.
    "},
    examples: Examples::new(&[
      r#" std.remove([1, 2, 3], 2) == [1, 3] "#,
      r#" std.remove([4, 2, 5, 2], 2) == [4, 5, 2] "#,
      r#" std.remove([6, 7], 2) == [6, 7] "#,
    ]),
  },
  Fn {
    name: S::new("removeAt"),
    implemented: false,
    sig: sig(&[ARR_ANY, req("idx", Ty::Num)], Ty::ArrAny),
    total: true,
    available_since: Some(21),
    doc: indoc! {"
      Removes the element at the index `idx` from `arr`.
    "},
    examples: Examples::new(&[
      r#" std.removeAt([1, 2, 3], 2) == [1, 2] "#,
      r#" std.removeAt([4, 8, 3, 9], 1) == [4, 3, 9] "#,
    ]),
  },
  Fn {
    name: S::new("set"),
    implemented: false,
    sig: sig(&[req("arr", Ty::ArrAny), KEY_F], Ty::SetAny),
    total: true,
    available_since: Some(10),
    doc: indoc! {"
      Returns a sorted array with no duplicates, i.e. a set.

      It is equivalent to `std.uniq(std.sort(arr))`.

      The optional `keyF` function can be used to extract a key to use from each element. This key
      is used for the purpose of identifying uniqueness.
    "},
    examples: Examples::new(&[r#" std.set([1, 6, 2, 6]) == [1, 2, 6] "#, r#" std.set([]) == [] "#]),
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
    examples: Examples::new(&[
      r#" std.setInter([1, 2], [2, 3]) == [2] "#,
      r#" std.setInter([1, 2], []) == []"#,
      r#" std.setInter([], [1, 2]) == []"#,
    ]),
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
    examples: Examples::new(&[
      "std.setUnion([1, 2], [2, 3]) == [1, 2, 3]",
      indoc! {r#"
        std.setUnion(
          [{n:"A", v:1}, {n:"B"}],
          [{n:"A", v: 9999}, {n:"C"}],
          keyF=function(x) x.n
        ) == [{ "n": "A", "v": 1 }, { "n": "B" }, { "n": "C" }]
      "#},
    ]),
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
    examples: Examples::new(&[
      r#" std.setDiff([1, 2], [2, 3]) == [1] "#,
      r#" std.setDiff([1, 2], []) == [1, 2] "#,
      r#" std.setDiff([], [1, 2]) == [] "#,
    ]),
  },
  Fn {
    name: S::new("setMember"),
    implemented: false,
    sig: sig(&[req("x", Ty::Any), req("arr", Ty::SetAny), KEY_F], Ty::Bool),
    total: true,
    available_since: Some(10),
    doc: indoc! {"
      Returns whether `x` is a member of `s`.

      `s` must be a set, i.e. a sorted array with no duplicates. If that is not the case, this
      function will quietly return non-meaningful results.

      The optional `keyF` function can be used to extract a key to use from each element. This key
      is used for the purpose of identifying uniqueness.
    "},
    examples: Examples::new(&[r#" std.setMember(1, [1, 2]) "#, r#" !std.setMember(3, [1, 2]) "#]),
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
    examples: Examples::new(&[
      r#" std.base64("hello world") == "aGVsbG8gd29ybGQ=" "#,
      r#" std.base64("") == "" "#,
    ]),
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
    examples: Examples::new(&[
      r#" std.decodeUTF8(std.base64DecodeBytes("aGVsbG8gd29ybGQ=")) == "hello world" "#,
      r#" std.base64DecodeBytes("") == [] "#,
    ]),
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
    examples: Examples::EMPTY_ALLOWED,
  },
  Fn {
    name: S::new("md5"),
    implemented: false,
    sig: sig(&[req("s", Ty::Str)], Ty::Str),
    total: true,
    available_since: Some(10),
    doc: indoc! {"
      Returns the MD5 hash of the string.
    "},
    examples: Examples::new(&[
      r#" std.md5("hello world") == "5eb63bbbe01eeed093cb22bb8f5acdc3" "#,
      r#" std.md5("") == "d41d8cd98f00b204e9800998ecf8427e" "#,
    ]),
  },
  Fn {
    name: S::new("sha1"),
    implemented: false,
    sig: HASH,
    total: true,
    available_since: Some(21),
    doc: indoc! {"
      Returns the SHA1 hash of the string.
    "},
    examples: Examples::new(&[
      indoc! {r#"
        std.sha1("hi")
          == "c22b5f9178342609428d6f51b2c5af4c0bde6a42"
      "#},
      indoc! {r#"
        std.sha1("bye")
          == "78c9a53e2f28b543ea62c8266acfdf36d5c63e61"
      "#},
    ]),
  },
  Fn {
    name: S::new("sha256"),
    implemented: false,
    sig: HASH,
    total: true,
    available_since: Some(21),
    doc: indoc! {"
      Returns the SHA256 hash of the string.
    "},
    examples: Examples::new(&[
      indoc! {r#"
        std.sha256("hi")
          == "8f434346648f6b96df89dda901c5176b10a6d83961dd3c1ac88b59b2dc327aa4"
      "#},
      indoc! {r#"
        std.sha256("bye")
          == "b49f425a7e1f9cff3856329ada223f2f9d368f15a00cf48df16ca95986137fe8"
      "#},
    ]),
  },
  Fn {
    name: S::new("sha3"),
    implemented: false,
    sig: HASH,
    total: true,
    available_since: Some(21),
    doc: indoc! {"
      Returns the SHA3 hash of the string.
    "},
    examples: Examples::new(&[
      indoc! {r#"
        std.sha3("hi")
          == "154013cb8140c753f0ac358da6110fe237481b26c75c3ddc1b59eaf9dd7b46a0a3aeb2cef164b3c82d65b38a4e26ea9930b7b2cb3c01da4ba331c95e62ccb9c3"
      "#},
      indoc! {r#"
        std.sha3("bye")
          == "22a7f800888da03800c5078d7116eacc78b0c75ae07b06c36e99a619eed763bcbe753dc7eab6a3bb2295b4ab52f793ccefa86b21386df476b9bf02dde9c4f12f"
      "#},
    ]),
  },
  Fn {
    name: S::new("sha512"),
    implemented: false,
    sig: HASH,
    total: true,
    available_since: Some(21),
    doc: indoc! {"
      Returns the SHA3 hash of the string.
    "},
    examples: Examples::new(&[
      indoc! {r#"
        std.sha512("hi")
          == "150a14ed5bea6cc731cf86c41566ac427a8db48ef1b9fd626664b3bfbb99071fa4c922f33dde38719b8c8354e2b7ab9d77e0e67fc12843920a712e73d558e197"
      "#},
      indoc! {r#"
        std.sha512("bye")
          == "23c9dee78e969bb483fdae563d681af010b77748dfbd959422abb792fa454db8e3d9cb4e1a3224066b349465f09a710d025e856db19da25d55c3928abef17c8c"
      "#},
    ]),
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
    examples: Examples::new(&[
      "!std.xor(true, true)",
      "std.xor(true, false)",
      "std.xor(false, true)",
      "!std.xor(false, false)",
    ]),
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
    examples: Examples::new(&[
      "std.xnor(true, true)",
      "!std.xnor(true, false)",
      "!std.xnor(false, true)",
      "std.xnor(false, false)",
    ]),
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
    examples: Examples::new(&[
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
    ]),
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
    examples: Examples::EMPTY_ALLOWED,
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
    examples: Examples::new(&[
      "std.equals(1 + 1, 2)",
      "std.equals('train' + 'ing', 'training')",
      "!std.equals(2 + 2, 5)",
    ]),
  },
  Fn {
    name: S::new("equalsIgnoreCase"),
    implemented: false,
    sig: sig(&[req("str1", Ty::Str), req("str2", Ty::Str)], Ty::Bool),
    total: true,
    available_since: Some(21),
    doc: indoc! {"
      Returns whether the strings are equal via case-insensitive comparison.
    "},
    examples: Examples::new(&[
      r#" std.equalsIgnoreCase("hi", "hi") "#,
      r#" std.equalsIgnoreCase("hello", "HeLLo") "#,
      r#" std.equalsIgnoreCase("hey", "HEY") "#,
      r#" !std.equalsIgnoreCase("hi", "bye") "#,
    ]),
  },
  Fn {
    name: S::new("objectHasEx"),
    implemented: true,
    sig: sig(
      &[req("obj", Ty::Obj), req("fname", Ty::StrInterned), req("hidden", Ty::Bool)],
      Ty::Bool,
    ),
    total: true,
    available_since: None,
    doc: indoc! {"
      Identical to:

      - `std.objectHasAll(obj, fname)` when `hidden` is `true`;
      - `std.objectHas(obj, fname)` when `hidden` is `false`.
    "},
    examples: Examples::new(&[
      r#" std.objectHasEx({a:: 1}, "a", true) "#,
      r#" !std.objectHasEx({a:: 1}, "a", false) "#,
    ]),
  },
];

/// A field (non-function).
#[derive(Debug)]
pub struct Field {
  /// The name.
  pub name: S,
  /// The type.
  pub ty: Ty,
  /// The documentation.
  pub doc: &'static str,
}

/// The fields (non-functions).
pub const FIELDS: [Field; 2] = [
  Field { name: S::new("thisFile"), ty: Ty::Str, doc: "The current filename." },
  Field {
    name: S::new("pi"),
    ty: Ty::Num,
    doc: "Archimedes' constant (π), the ratio of a circle's circumference to its diameter.",
  },
];
