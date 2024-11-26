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
  /// When this is available since. If Some(n), this is available since Jsonnet version 0.n.0.
  pub available_since: Option<u8>,
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
    available_since: Some(10),
    doc: indoc! {"
      If an external variable with the given name was defined, return its value. Otherwise, raise
      an error.
    "},
  },
  Fn {
    name: S::named("type", "type_"),
    sig: sig(&[req("x", Ty::Any)], Ty::StaticStr),
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

      ```jsonnet
      assert std.type([1]) == "array";
      assert std.type(null) == "null";
      assert std.type({}) == "object";
      assert std.type(3) == "number";
      ```
    "#},
  },
  Fn {
    name: S::new("isArray"),
    sig: V_ANY_RET_BOOL,
    total: true,
    available_since: None,
    doc: indoc! {"
      Returns whether the argument is an array.

      ```jsonnet
      assert std.isArray([1, 2]);
      assert std.isArray([]);
      assert !std.isArray(null);
      assert !std.isArray(4);
      ```
    "},
  },
  Fn {
    name: S::new("isBoolean"),
    sig: V_ANY_RET_BOOL,
    total: true,
    available_since: None,
    doc: indoc! {"
      Returns whether the argument is a boolean.

      ```jsonnet
      assert std.isBoolean(true);
      assert std.isBoolean(false);
      assert !std.isBoolean(null);
      assert !std.isBoolean(4);
      ```
    "},
  },
  Fn {
    name: S::new("isFunction"),
    sig: V_ANY_RET_BOOL,
    total: true,
    available_since: None,
    doc: indoc! {"
      Returns whether the argument is a function.

      ```jsonnet
      assert std.isFunction(function(x) x + 1);
      assert std.isFunction(std.mod);
      assert !std.isFunction(null);
      assert !std.isFunction(4);
      ```
    "},
  },
  Fn {
    name: S::new("isNumber"),
    sig: V_ANY_RET_BOOL,
    total: true,
    available_since: None,
    doc: indoc! {"
      Returns whether the argument is a number.

      ```jsonnet
      assert std.isNumber(3);
      assert std.isNumber(-123.345);
      assert !std.isNumber(null);
      assert !std.isNumber([]);
      ```
    "},
  },
  Fn {
    name: S::new("isObject"),
    sig: V_ANY_RET_BOOL,
    total: true,
    available_since: None,
    doc: indoc! {"
      Returns whether the argument is an object.

      ```jsonnet
      assert std.isObject({});
      assert std.isObject({ a: 1 } + { b: 2 });
      assert !std.isObject(null);
      assert !std.isObject([]);
      ```
    "},
  },
  Fn {
    name: S::new("isString"),
    sig: V_ANY_RET_BOOL,
    total: true,
    available_since: None,
    doc: indoc! {r#"
      Returns whether the argument is a string.

      ```jsonnet
      assert std.isString("hi");
      assert std.isString("");
      assert !std.isString(null);
      assert !std.isString({});
      ```
    "#},
  },
  Fn {
    name: S::new("length"),
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

      ```jsonnet
      assert std.length("hi") == 2;
      assert std.length("") == 0;
      assert std.length("あ") == 1;

      assert std.length([]) == 0;
      assert std.length([3, 4]) == 2;

      assert std.length(function(x) x + 1) == 1;
      assert std.length(function() 4) == 0;
      assert std.length(function(x=1) x + 2) == 0;

      assert std.length({}) == 0;
      assert std.length({ a: 3, b: 5 }) == 2;
      assert std.length({ x:: 9, y::: 7 }) == 2;
      ```
    "#},
  },
  Fn {
    name: S::new("get"),
    sig: sig(
      &[req("o", Ty::Obj), req("f", Ty::Str), opt("default", Ty::Any), opt("inc_hidden", Ty::Bool)],
      Ty::Any,
    ),
    total: true,
    available_since: None,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("objectHas"),
    sig: OBJ_HAS,
    total: true,
    available_since: None,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("objectFields"),
    sig: OBJ_FIELDS,
    total: true,
    available_since: None,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("objectValues"),
    sig: OBJ_VALUES,
    total: true,
    available_since: None,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("objectKeysValues"),
    sig: OBJ_KEYS_VALUES,
    total: true,
    available_since: None,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("objectHasAll"),
    sig: OBJ_HAS,
    total: true,
    available_since: None,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("objectFieldsAll"),
    sig: OBJ_FIELDS,
    total: true,
    available_since: None,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("objectValuesAll"),
    sig: OBJ_VALUES,
    total: true,
    available_since: None,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("objectKeysValuesAll"),
    sig: OBJ_KEYS_VALUES,
    total: true,
    available_since: None,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("prune"),
    sig: sig(&[req("a", Ty::Any)], Ty::Any),
    total: true,
    available_since: Some(10),
    doc: indoc! {r#"
      Recursively remove all "empty" members of a. "Empty" is defined as

      - zero length arrays
      - zero length objects
      - `null` values

      The argument may have any type.

      <!-- @eval-error: not yet implemented: prune -->

      ```jsonnet
      assert std.prune([1, [], 2, {}, 3, null]) == [1, 2, 3];
      assert std.prune({a: 3}) == {a: 3};
      assert std.prune({w: 0, x: "", y: [], z: null}) == {w: 0, x: ""};
      assert std.prune(null) == null;
      ```
    "#},
  },
  Fn {
    name: S::new("mapWithKey"),
    sig: sig(&[req("func", Ty::Hof2), req("obj", Ty::Obj)], Ty::Obj),
    total: true,
    available_since: None,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("abs"),
    sig: N_NUM_RET_NUM,
    total: true,
    available_since: None,
    doc: indoc! {"
      Returns the absolute value of the number.

      ```jsonnet
      assert std.abs(3) == 3;
      assert std.abs(-1.2) == 1.2;
      assert std.abs(0) == 0;
      ```
    "},
  },
  Fn {
    name: S::new("sign"),
    sig: N_NUM_RET_NUM,
    total: true,
    available_since: None,
    doc: indoc! {"
      Returns `-1`, `0`, or `1` if the number is negative, zero, or positive respectively.

      ```jsonnet
      assert std.sign(3) == 1;
      assert std.sign(-1.2) == -1;
      assert std.sign(0) == 0;
      ```
    "},
  },
  Fn {
    name: S::new("max"),
    sig: sig(&[req("a", Ty::Num), req("b", Ty::Num)], Ty::Num),
    total: true,
    available_since: None,
    doc: indoc! {"
      Returns the maximum of the two arguments.

      ```jsonnet
      assert std.max(3, 2) == 3;
      assert std.max(4, 4) == 4;
      assert std.max(-5, 1) == 1;
      ```
    "},
  },
  Fn {
    name: S::new("min"),
    sig: sig(&[req("a", Ty::Num), req("b", Ty::Num)], Ty::Num),
    total: true,
    available_since: None,
    doc: indoc! {"
      Returns the minimum of the two arguments.

      ```jsonnet
      assert std.min(3, 2) == 2;
      assert std.min(4, 4) == 4;
      assert std.min(-5, 1) == -5;
      ```
    "},
  },
  Fn {
    name: S::new("pow"),
    sig: sig(&[req("x", Ty::Num), req("n", Ty::Num)], Ty::Num),
    total: true,
    available_since: None,
    doc: indoc! {"
      `std.pow(x, y)` returns $x^y$, i.e. $x$ to the $y$ power.

      ```jsonnet
      assert std.pow(2, 3) == 8;
      assert std.pow(3, 2) == 9;
      assert std.pow(1, 99) == 1;
      assert std.pow(0, 2) == 0;
      assert std.pow(99, 0) == 1;
      ```
    "},
  },
  Fn {
    name: S::new("exp"),
    sig: X_NUM_RET_NUM,
    total: true,
    available_since: None,
    doc: indoc! {r"
      `std.exp(x)` returns $e^x$, i.e. $e$ to the $x$ power, where
      [$e \approx 2.71828$](<https://en.wikipedia.org/wiki/E_(mathematical_constant)>).

      ```jsonnet
      assert std.exp(0) == 1;
      assert std.exp(1) == 2.718281828459045;
      assert std.exp(2) == 7.38905609893065;
      ```
    "},
  },
  Fn {
    name: S::new("log"),
    sig: X_NUM_RET_NUM,
    total: true,
    available_since: None,
    doc: indoc! {r"
      `std.log(x)` returns the natural logarithm of $x$,
      i.e. the solution $y$ in $e^y = x$, where
      [$e \approx 2.71828$](<https://en.wikipedia.org/wiki/E_(mathematical_constant)>).

      ```jsonnet
      assert std.log(1) == 0;
      assert std.log(123) == 4.812184355372417;
      assert std.log(345) == 5.84354441703136;
      ```
    "},
  },
  Fn {
    name: S::new("exponent"),
    sig: X_NUM_RET_NUM,
    total: true,
    available_since: None,
    doc: indoc! {"
      `std.exponent(x)` returns the exponent of the IEEE754 64-bit floating point number `x`.

      The following function returns `true` for all numbers `x`:

      <!-- @eval-error: manifest function -->

      ```jsonnet
      function(x)
        assert std.isNumber(x);
        x == std.mantissa(x) * std.pow(2, std.exponent(x))
      ```
    "},
  },
  Fn {
    name: S::new("mantissa"),
    sig: X_NUM_RET_NUM,
    total: true,
    available_since: None,
    doc: indoc! {"
      `std.mantissa(x)` returns the significand, also called the mantissa, of the IEEE754
      64-bit floating point number `x`.

      The following function returns `true` for all numbers `x`:

      <!-- @eval-error: manifest function -->

      ```jsonnet
      function(x)
        assert std.isNumber(x);
        x == std.mantissa(x) * std.pow(2, std.exponent(x))
      ```
    "},
  },
  Fn {
    name: S::new("floor"),
    sig: X_NUM_RET_NUM,
    total: true,
    available_since: None,
    doc: indoc! {"
      Returns the smallest integer greater than or equal to the argument.

      ```jsonnet
      assert std.floor(1) == 1;
      assert std.floor(1.99) == 1;
      assert std.floor(2.01) == 2;
      assert std.floor(-1) == -1;
      assert std.floor(-1.01) == -2;
      assert std.floor(-1.99) == -2;
      assert std.floor(-2.01) == -3;
      ```
    "},
  },
  Fn {
    name: S::new("ceil"),
    sig: X_NUM_RET_NUM,
    total: true,
    available_since: None,
    doc: indoc! {"
      Returns the greatest integer smaller than or equal to the argument.

      ```jsonnet
      assert std.ceil(1) == 1;
      assert std.ceil(1.99) == 2;
      assert std.ceil(2.01) == 3;
      assert std.ceil(-1) == -1;
      assert std.ceil(-1.01) == -1;
      assert std.ceil(-1.99) == -1;
      assert std.ceil(-2.01) == -2;
      ```
    "},
  },
  Fn {
    name: S::new("sqrt"),
    sig: X_NUM_RET_NUM,
    total: true,
    available_since: None,
    doc: indoc! {"
      Returns the square root of the argument.

      ```jsonnet
      assert std.sqrt(9) == 3;
      assert std.sqrt(4) == 2;
      assert std.sqrt(1) == 1;
      ```
    "},
  },
  Fn {
    name: S::new("sin"),
    sig: X_NUM_RET_NUM,
    total: true,
    available_since: None,
    doc: indoc! {"
      Returns the sine of the argument.

      ```jsonnet
      assert std.sin(0.5) == 0.479425538604203;
      ```
    "},
  },
  Fn {
    name: S::new("cos"),
    sig: X_NUM_RET_NUM,
    total: true,
    available_since: None,
    doc: indoc! {"
      Returns the cosine of its argument.

      ```jsonnet
      assert std.cos(0.5) == 0.8775825618903728;
      ```
    "},
  },
  Fn {
    name: S::new("tan"),
    sig: X_NUM_RET_NUM,
    total: true,
    available_since: None,
    doc: indoc! {"
      Returns the tangent of its argument.

      ```jsonnet
      assert std.tan(0.5) == 0.5463024898437905;
      ```
    "},
  },
  Fn {
    name: S::new("asin"),
    sig: X_NUM_RET_NUM,
    total: true,
    available_since: None,
    doc: indoc! {"
      Returns the arcsine of its argument.

      ```jsonnet
      assert std.asin(0.5) == 0.5235987755982988;
      ```
    "},
  },
  Fn {
    name: S::new("acos"),
    sig: X_NUM_RET_NUM,
    total: true,
    available_since: None,
    doc: indoc! {"
      Returns the arccosine of its argument.

      ```jsonnet
      assert std.acos(0.5) == 1.0471975511965976;
      ```
    "},
  },
  Fn {
    name: S::new("atan"),
    sig: X_NUM_RET_NUM,
    total: true,
    available_since: None,
    doc: indoc! {"
      Returns the arctangent of its argument.

      ```jsonnet
      assert std.atan(0.5) == 0.46364760900080615;
      ```
    "},
  },
  Fn {
    name: S::new("round"),
    sig: X_NUM_RET_NUM,
    total: true,
    available_since: None,
    doc: indoc! {"
      Returns the argument rounded to the nearest integer.

      ```jsonnet
      assert std.round(1) == 1;
      assert std.round(1.1) == 1;
      assert std.round(1.5) == 2;
      assert std.round(1.9) == 2;
      assert std.round(-1) == -1;
      assert std.round(-1.1) == -1;
      assert std.round(-1.5) == -2;
      assert std.round(-1.9) == -2;
      ```
    "},
  },
  Fn {
    name: S::new("isEven"),
    sig: X_NUM_RET_BOOL,
    total: true,
    available_since: None,
    doc: indoc! {"
      Returns whether the argument is an even number.

      Raises if the argument is not a number.

      ```jsonnet
      assert std.isEven(2);
      assert std.isEven(0);
      assert std.isEven(-2);
      assert !std.isEven(1);
      assert !std.isEven(9);
      assert !std.isEven(-5);
      assert !std.isEven(4.4);
      assert !std.isEven(5.5);
      ```
    "},
  },
  Fn {
    name: S::new("isOdd"),
    sig: X_NUM_RET_BOOL,
    total: true,
    available_since: None,
    doc: indoc! {"
      Returns whether the argument is an odd number.

      Raises if the argument is not a number.

      ```jsonnet
      assert std.isOdd(1);
      assert std.isOdd(9);
      assert std.isOdd(-5);
      assert !std.isOdd(2);
      assert !std.isOdd(0);
      assert !std.isOdd(-2);
      assert !std.isOdd(4.4);
      assert !std.isOdd(5.5);
      ```
    "},
  },
  Fn {
    name: S::new("isInteger"),
    sig: X_NUM_RET_BOOL,
    total: true,
    available_since: None,
    doc: indoc! {"
      Returns whether the argument is an integer number.

      Raises if the argument is not a number.

      ```jsonnet
      assert std.isInteger(1);
      assert std.isInteger(0);
      assert std.isInteger(-5);
      assert !std.isInteger(0.1);
      assert !std.isInteger(4.4);
      assert !std.isInteger(2.0001);
      ```
    "},
  },
  Fn {
    name: S::new("isDecimal"),
    sig: X_NUM_RET_BOOL,
    total: true,
    available_since: None,
    doc: indoc! {"
      Returns whether the argument is a decimal number, i.e. one with non-zero digits
      after the decimal point.

      Raises if the argument is not a number.

      ```jsonnet
      assert std.isDecimal(0.1);
      assert std.isDecimal(4.4);
      assert std.isDecimal(2.0001);
      assert !std.isDecimal(1);
      assert !std.isDecimal(0);
      assert !std.isDecimal(-5);
      ```
    "},
  },
  Fn {
    name: S::named("mod", "mod_"),
    sig: sig(&[req("a", Ty::NumOrStr), req("b", Ty::Any)], Ty::Any),
    total: false,
    available_since: None,
    doc: indoc! {"
      This is what the `%` operator is desugared to. It performs modulo arithmetic if the left
      hand side is a number, or if the left hand side is a string, it does Python-style string
      formatting with `std.format`.
    "},
  },
  Fn {
    name: S::new("clamp"),
    sig: sig(&[req("x", Ty::Num), req("minVal", Ty::Num), req("maxVal", Ty::Num)], Ty::Num),
    total: true,
    available_since: Some(15),
    doc: indoc! {"
      `std.clamp(x, minVal, maxVal)` clamps a value to fit within the range `[minVal, maxVal]`.

      Equivalent to `std.max(minVal, std.min(x, maxVal))`.

      ```jsonnet
      assert std.clamp(-3, 0, 5) == 0;
      assert std.clamp(4, 0, 5) == 4;
      assert std.clamp(7, 0, 5) == 5;
      ```
    "},
  },
  Fn {
    name: S::new("assertEqual"),
    sig: sig(&[req("a", Ty::Any), req("b", Ty::Any)], Ty::True),
    total: true,
    available_since: Some(10),
    doc: indoc! {"
      `std.assertEqual(a, b)` ensures that `a == b` holds.

      Returns `true` if so, else throws an error message.
    "},
  },
  Fn {
    name: S::new("toString"),
    sig: sig(&[req("a", Ty::Any)], Ty::Str),
    total: true,
    available_since: Some(10),
    doc: indoc! {"
      Converts the given argument to a string.
    "},
  },
  Fn {
    name: S::new("codepoint"),
    sig: sig(&[req("str", Ty::Str)], Ty::Uint),
    total: true,
    available_since: Some(10),
    doc: indoc! {"
      Returns the positive integer representing the unicode codepoint of the character
      in the given single-character string.

      This function is the inverse of `std.char`.
    "},
  },
  Fn {
    name: S::new("char"),
    sig: sig(&[req("n", Ty::Uint)], Ty::Str),
    total: true,
    available_since: Some(10),
    doc: indoc! {"
      Returns a string of length one whose only unicode codepoint has integer id n.

      This function is the inverse of `std.codepoint`.
    "},
  },
  Fn {
    name: S::new("substr"),
    sig: sig(&[req("str", Ty::Str), req("from", Ty::Uint), req("len", Ty::Uint)], Ty::Str),
    total: false,
    available_since: Some(10),
    doc: indoc! {r#"
      `std.substr(str, from, len)` returns a string that is the part of `s` that starts at
      offset `from` and is `len` codepoints long.

      If the string `s` is shorter than `from + len`, returns the suffix starting at position
      `from`.

      ```jsonnet
      assert std.substr("think", 1, 2) == "hi";
      assert std.substr("develop", 4, 3) == "lop";
      assert std.substr("hello world", 6, 99) == "world";
      ```
    "#},
  },
  Fn {
    name: S::new("findSubstr"),
    sig: sig(&[req("pat", Ty::Str), req("str", Ty::Str)], Ty::ArrNum),
    total: true,
    available_since: Some(10),
    doc: indoc! {r#"
      `std.findSubstr(pat, str)` returns an array that contains the indexes of all occurrences
      of `pat` in `str`.

      <!-- @eval-error: not yet implemented -->

      ```jsonnet
      assert std.findSubstr("e", "envelope") == [0, 3, 7];
      assert std.findSubstr("hi", "hi Chidi") == [0, 4];
      assert std.findSubstr("fork", "shirt") == [];
      ```
    "#},
  },
  Fn {
    name: S::new("startsWith"),
    sig: A_B_STR_RET_BOOL,
    total: true,
    available_since: Some(10),
    doc: indoc! {r#"
      `std.startsWith(a, b)` returns whether the string `a` is prefixed by the string `b`.

      ```jsonnet
      assert std.startsWith("hi Chidi", "hi");
      assert !std.startsWith("hi Chidi", "fork");
      ```
    "#},
  },
  Fn {
    name: S::new("endsWith"),
    sig: A_B_STR_RET_BOOL,
    total: true,
    available_since: Some(10),
    doc: indoc! {r#"
      `std.endsWith(a, b)` returns whether the string `a` is suffixed by the string `b`.

      ```jsonnet
      assert std.endsWith("thank you", "you");
      assert !std.endsWith("thank you", "no");
      ```
    "#},
  },
  Fn {
    name: S::new("stripChars"),
    sig: STR_CHARS_STR_RET_STR,
    total: true,
    available_since: Some(15),
    doc: indoc! {r#"
      `std.stripChars(str, chars)` removes characters `chars` from the beginning and from the end
      of `str`.

      ```jsonnet
      assert std.stripChars(" test test test ", " ") == "test test test";
      assert std.stripChars("aaabbbbcccc", "ac") == "bbbb";
      assert std.stripChars("cacabbbbaacc", "ac") == "bbbb";
      ```
    "#},
  },
  Fn {
    name: S::new("lstripChars"),
    sig: STR_CHARS_STR_RET_STR,
    total: true,
    available_since: Some(15),
    doc: indoc! {r#"
      `std.lstripChars(str, chars)` removes characters `chars` from the beginning of `str`.

      ```jsonnet
      assert std.lstripChars(" test test test ", " ") == "test test test ";
      assert std.lstripChars("aaabbbbcccc", "ac") == "bbbbcccc";
      assert std.lstripChars("cacabbbbaacc", "ac") == "bbbbaacc";
      ```
    "#},
  },
  Fn {
    name: S::new("rstripChars"),
    sig: STR_CHARS_STR_RET_STR,
    total: true,
    available_since: Some(15),
    doc: indoc! {r#"
      `std.rstripChars(str, chars)` removes characters `chars` from the end of `str`.

      ```jsonnet
      assert std.rstripChars(" test test test ", " ") == " test test test";
      assert std.rstripChars("aaabbbbcccc", "ac") == "aaabbbb";
      assert std.rstripChars("cacabbbbaacc", "ac") == "cacabbbb";
      ```
    "#},
  },
  Fn {
    name: S::new("split"),
    sig: sig(&[req("str", Ty::Str), req("c", Ty::Str)], Ty::ArrStr),
    total: true,
    available_since: Some(10),
    doc: indoc! {r#"
      `std.split(str, c)` splits the string `str` into an array of strings, divided by the
      string `c`.

      Note: Versions up to and including 0.18.0 require `c` to be a single character.

      <!-- @eval-error: not yet implemented -->

      ```jsonnet
      assert std.split("foo/_bar", "/_") == [ "foo", "bar" ];
      assert std.split("/_foo/_bar", "/_") == [ "", "foo", "bar" ];
      ```
    "#},
  },
  Fn {
    name: S::new("splitLimit"),
    sig: SPLIT_LIMIT,
    total: true,
    available_since: Some(10),
    doc: indoc! {r#"
      `std.splitLimit(str, c, maxsplits)` is the same as `std.split(str, c)` but will stop
      after `maxsplits` splits, thereby the largest array it will return has
      length `maxsplits + 1`. A limit of `-1` means unlimited.

      Note: Versions up to and including 0.18.0 require `c` to be a single character.

      <!-- @eval-error: not yet implemented -->

      ```jsonnet
      assert std.splitLimit("foo/_bar", "/_", 1) == [ "foo", "bar" ];
      assert std.splitLimit("/_foo/_bar", "/_", 1) == [ "", "foo/_bar" ];
      ```
    "#},
  },
  Fn {
    name: S::new("splitLimitR"),
    sig: SPLIT_LIMIT,
    total: true,
    available_since: Some(19),
    doc: indoc! {r#"
      `std.splitLimitR(str, c, maxsplits)` is the
      same as `std.splitLimit(str, c, maxsplits)` but will split from right to left.

      <!-- @eval-error: not yet implemented -->

      ```jsonnet
      assert std.splitLimitR("/_foo/_bar", "/_", 1) == [ "/_foo", "bar" ];
      ```
    "#},
  },
  Fn {
    name: S::new("strReplace"),
    sig: sig(&[req("str", Ty::Str), req("from", Ty::Str), req("to", Ty::Str)], Ty::Str),
    total: true,
    available_since: Some(10),
    doc: indoc! {r#"
      `std.strReplace(str, from, to)` returns a copy of the string `str` in which all occurrences
      of string `from` have been replaced with string `to`.

      ```jsonnet
      assert std.strReplace("I like to skate with my skateboard", "skate", "surf")
        == "I like to surf with my surfboard";
      ```
    "#},
  },
  Fn {
    name: S::new("isEmpty"),
    sig: sig(&[req("str", Ty::Str)], Ty::Bool),
    total: true,
    available_since: Some(20),
    doc: indoc! {r#"
      Returns whether the given string has zero length.

      ```jsonnet
      assert std.isEmpty("");
      assert !std.isEmpty("hi");
      ```
    "#},
  },
  Fn {
    name: S::new("asciiUpper"),
    sig: STR_RET_STR,
    total: true,
    available_since: Some(10),
    doc: indoc! {r#"
      Returns a copy of the string in which all ASCII letters are capitalized.

      ```jsonnet
      assert std.asciiUpper("100 Cats!") == "100 CATS!";
      ```
    "#},
  },
  Fn {
    name: S::new("asciiLower"),
    sig: STR_RET_STR,
    total: true,
    available_since: Some(10),
    doc: indoc! {r#"
      Returns a copy of the string in which all ASCII letters are lower cased.

      ```jsonnet
      assert std.asciiLower("100 Cats!") == "100 cats!";
      ```
    "#},
  },
  Fn {
    name: S::new("stringChars"),
    sig: sig(&[req("str", Ty::Str)], Ty::ArrStr),
    total: true,
    available_since: Some(10),
    doc: indoc! {r#"
      Split the string into an array of strings, each containing a single codepoint.

      <!-- @eval-error: not yet implemented -->

      ```jsonnet
      assert std.stringChars("foo") == ["f", "o", "o"];
      ```
    "#},
  },
  Fn {
    name: S::new("format"),
    sig: sig(&[req("str", Ty::Str), req("vals", Ty::Any)], Ty::Str),
    total: true,
    available_since: None,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("escapeStringBash"),
    sig: STR_RET_STR,
    total: true,
    available_since: None,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("escapeStringDollars"),
    sig: STR_RET_STR,
    total: true,
    available_since: None,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("escapeStringJson"),
    sig: STR_RET_STR,
    total: true,
    available_since: None,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("escapeStringPython"),
    sig: STR_RET_STR,
    total: true,
    available_since: None,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("escapeStringXml"),
    sig: STR_RET_STR,
    total: true,
    available_since: None,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("parseInt"),
    sig: STR_RET_NUM,
    total: true,
    available_since: None,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("parseOctal"),
    sig: STR_RET_NUM,
    total: true,
    available_since: None,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("parseHex"),
    sig: STR_RET_NUM,
    total: true,
    available_since: None,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("parseJson"),
    sig: STR_RET_ANY,
    total: true,
    available_since: None,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("parseYaml"),
    sig: STR_RET_ANY,
    total: true,
    available_since: None,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("encodeUTF8"),
    sig: sig(&[req("str", Ty::Str)], Ty::ArrNum),
    total: true,
    available_since: None,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("decodeUTF8"),
    sig: sig(&[req("arr", Ty::ArrNum)], Ty::Str),
    total: true,
    available_since: None,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("manifestIni"),
    sig: sig(&[req("ini", Ty::Obj)], Ty::Str),
    total: true,
    available_since: None,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("manifestPython"),
    sig: sig(&[req("v", Ty::Any)], Ty::Str),
    total: true,
    available_since: None,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("manifestPythonVars"),
    sig: sig(&[req("conf", Ty::Any)], Ty::Str),
    total: true,
    available_since: None,
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
    available_since: None,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("manifestJson"),
    sig: MANIFEST_JSON,
    total: true,
    available_since: None,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("manifestJsonMinified"),
    sig: MANIFEST_JSON,
    total: true,
    available_since: None,
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
    available_since: None,
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
    available_since: None,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("manifestXmlJsonml"),
    sig: sig(&[req("value", Ty::ArrAny)], Ty::Str),
    total: true,
    available_since: None,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("manifestTomlEx"),
    sig: sig(&[req("toml", Ty::Obj), req("indent", Ty::Str)], Ty::Str),
    total: true,
    available_since: None,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("makeArray"),
    sig: sig(&[req("sz", Ty::Uint), req("func", Ty::Hof1)], Ty::ArrAny),
    total: true,
    available_since: None,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("member"),
    sig: sig(&[req("arr", Ty::StrOrArrAny), req("x", Ty::Any)], Ty::Bool),
    total: true,
    available_since: None,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("count"),
    sig: sig(&[req("arr", Ty::ArrAny), req("x", Ty::Any)], Ty::Num),
    total: true,
    available_since: None,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("find"),
    sig: sig(&[req("value", Ty::Any), req("arr", Ty::ArrAny)], Ty::ArrNum),
    total: true,
    available_since: None,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("map"),
    sig: ARR_HOF1,
    total: true,
    available_since: None,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("mapWithIndex"),
    sig: sig(&[req("func", Ty::Hof2), req("arr", Ty::ArrAny)], Ty::ArrAny),
    total: true,
    available_since: None,
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
    available_since: None,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("flatMap"),
    sig: sig(&[req("func", Ty::Hof1), req("arr", Ty::StrOrArrAny)], Ty::StrOrArrAny),
    total: true,
    available_since: None,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("filter"),
    sig: ARR_HOF1,
    total: true,
    available_since: None,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("foldl"),
    sig: FOLD,
    total: true,
    available_since: None,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("foldr"),
    sig: FOLD,
    total: true,
    available_since: None,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("range"),
    sig: sig(&[req("from", Ty::Num), req("to", Ty::Num)], Ty::ArrNum),
    total: true,
    available_since: None,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("repeat"),
    sig: sig(&[req("what", Ty::StrOrArrAny), req("count", Ty::Uint)], Ty::StrOrArrAny),
    total: true,
    available_since: None,
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
    available_since: None,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("join"),
    sig: sig(&[req("sep", Ty::StrOrArrAny), req("arr", Ty::ArrAny)], Ty::StrOrArrAny),
    total: false,
    available_since: None,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("lines"),
    sig: sig(&[req("arr", Ty::ArrStr)], Ty::Str),
    total: true,
    available_since: None,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("flattenArrays"),
    sig: sig(&[req("arr", Ty::ArrAny)], Ty::ArrAny),
    total: true,
    available_since: None,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("reverse"),
    sig: sig(&[req("arr", Ty::ArrAny)], Ty::ArrAny),
    total: true,
    available_since: None,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("sort"),
    sig: ARR_KEY_F,
    total: true,
    available_since: None,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("uniq"),
    sig: ARR_KEY_F,
    total: true,
    available_since: None,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("all"),
    sig: sig(&[req("arr", Ty::ArrBool)], Ty::Bool),
    total: true,
    available_since: None,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("any"),
    sig: sig(&[req("arr", Ty::ArrBool)], Ty::Bool),
    total: true,
    available_since: None,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("sum"),
    sig: sig(&[req("arr", Ty::ArrNum)], Ty::Num),
    total: true,
    available_since: None,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("avg"),
    sig: sig(&[req("arr", Ty::ArrNum)], Ty::Num),
    total: true,
    available_since: None,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("set"),
    sig: ARR_KEY_F,
    total: true,
    available_since: None,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("setInter"),
    sig: BINARY_SET_FN,
    total: true,
    available_since: None,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("setUnion"),
    sig: BINARY_SET_FN,
    total: true,
    available_since: None,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("setDiff"),
    sig: BINARY_SET_FN,
    total: true,
    available_since: None,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("setMember"),
    sig: sig(&[req("x", Ty::Any), req("arr", Ty::ArrAny), opt("keyF", Ty::Hof1)], Ty::Bool),
    total: true,
    available_since: None,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("base64"),
    sig: sig(&[req("input", Ty::StrOrArrNum)], Ty::Str),
    total: true,
    available_since: None,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("base64DecodeBytes"),
    sig: sig(&[req("str", Ty::Str)], Ty::ArrNum),
    total: true,
    available_since: None,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("base64Decode"),
    sig: STR_RET_STR,
    total: true,
    available_since: None,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("md5"),
    sig: sig(&[req("s", Ty::Str)], Ty::Str),
    total: true,
    available_since: None,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("xor"),
    sig: X_Y_BOOL_RET_BOOL,
    total: true,
    available_since: None,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("xnor"),
    sig: X_Y_BOOL_RET_BOOL,
    total: true,
    available_since: None,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("mergePatch"),
    sig: sig(&[req("target", Ty::Any), req("patch", Ty::Any)], Ty::Any),
    total: true,
    available_since: None,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("trace"),
    sig: sig(&[req("str", Ty::Str), req("rest", Ty::Any)], Ty::Any),
    total: true,
    available_since: None,
    doc: indoc! {"
      TODO
    "},
  },
  // alluded to in the spec but not mentioned on the std lib page
  Fn {
    name: S::new("equals"),
    sig: sig(&[req("x", Ty::Any), req("y", Ty::Any)], Ty::Bool),
    total: false,
    available_since: None,
    doc: indoc! {"
      TODO
    "},
  },
  Fn {
    name: S::new("objectHasEx"),
    sig: sig(&[req("obj", Ty::Obj), req("fname", Ty::Str), req("hidden", Ty::Bool)], Ty::Bool),
    total: true,
    available_since: None,
    doc: indoc! {"
      TODO
    "},
  },
];
