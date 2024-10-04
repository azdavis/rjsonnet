# Standard library

These are docs for the Jsonnet standard library, available on the global `std` object.

- Retrieved from https://jsonnet.org/ref/stdlib.html
- Applied some formatting improvements
- Reworded a bit
- Removed some unwieldy examples
- Filled in some doc of my own for less-documented functions
- Removed doc for not yet implemented functions

## `extVar`

_Available since version 0.10.0._

If an external variable with the given name was defined, return its value. Otherwise, raise an error.

## `thisFile`

_Available since version 0.10.0._

Note that this is a field. It contains the current Jsonnet filename as a string.

## `type`

_Available since version 0.10.0._

Return a string that indicates the type of the value. The possible return values are

- `"array"`
- `"boolean"`
- `"function"`
- `"null"`
- `"number"`
- `"object"`
- `"string"`

## `isArray`

Returns `true` if the argument is an array, else `false`.

## `isBoolean`

Returns `true` if the argument is a boolean, else `false`.

## `isFunction`

Returns `true` if the argument is a function, else `false`.

## `isNumber`

Returns `true` if the argument is a number, else `false`.

## `isObject`

Returns `true` if the argument is an object, else `false`.

## `isString`

Returns `true` if the argument is a string, else `false`.

## `length`

_Available since version 0.10.0._

Depending on the type of the value given, this functions returns the number of _something_ in that argument value. The table below describes the _something_:

| Type     | Something  |
| -------- | ---------- |
| array    | elements   |
| string   | codepoints |
| function | parameters |
| object   | fields     |

Raises an error if given `null`, `true`, `false`, or a number.

## `prune`

_Available since version 0.10.0._

Recursively remove all "empty" members of a. "Empty" is defined as

- zero length arrays
- zero length objects
- `null` values

The argument may have any type.

## `abs`

Returns the absolute value of the number.

## `sign`

Returns `-1`, `0`, or `1` if the number is negative, zero, or positive respectively.

## `max`

Returns the maximum of the two arguments.

## `min`

Returns the minimum of the two arguments.

## `pow`

`std.pow(x, y)` returns $x^y$, i.e. $x$ to the $y$ power.

## `exp`

`std.exp(x)` returns $e^x$, i.e. $e$ to the $x$ power, where [$e \approx 2.71828$](<https://en.wikipedia.org/wiki/E_(mathematical_constant)>).

## `log`

`std.log(x)` returns the natural logarithm of $x$, i.e. the solution $y$ in $e^y = x$, where [$e \approx 2.71828$](<https://en.wikipedia.org/wiki/E_(mathematical_constant)>).

## `exponent`

Returns TODO

## `mantissa`

Returns TODO

## `floor`

Returns the smallest integer greater than or equal to the argument.

## `ceil`

Returns the greatest integer smaller than or equal to the argument.

## `sqrt`

Returns the square root of the argument.

## `sin`

Returns the sine of the argument.

This is a trigonometry function.

## `cos`

Returns the cosine of its argument.

This is a trigonometry function.

## `tan`

Returns the tangent of its argument.

This is a trigonometry function.

## `asin`

Returns the arcsine of its argument.

This is a trigonometry function.

## `acos`

Returns the arccosine of its argument.

This is a trigonometry function.

## `atan`

Returns the arctangent of its argument.

This is a trigonometry function.

## `round`

Returns the argument rounded to the nearest integer.

## `isEven`

Returns `true` if the argument is an even number, else `false`.

Raises if the argument is not a number.

## `isOdd`

Returns `true` if the argument is an odd number, else `false`.

Raises if the argument is not a number.

## `isInteger`

Returns `true` if the argument is an integer number, else `false`.

Raises if the argument is not a number.

## `isDecimal`

Returns `true` if the argument is a decimal number (i.e. one with non-zero digits after the decimal point), else `false`.

Raises if the argument is not a number.

## `mod`

This is what the `%` operator is desugared to. It performs modulo arithmetic if the left hand side is a number, or if the left hand side is a string, it does Python-style string formatting with `std.format`.

## `clamp`

_Available since version 0.15.0._

`std.clamp(x, minVal, maxVal)` clamps a value to fit within the range `[minVal,maxVal]`.

Equivalent to `std.max(minVal, std.min(x, maxVal))`.

Examples:

```jsonnet
std.clamp(-3, 0, 5) == 0
std.clamp(4, 0, 5) == 4
std.clamp(7, 0, 5) == 5
```

## `assertEqual`

_Available since version 0.10.0._

`std.assertEqual(a, b)` ensures that `a == b` holds. Returns `true` if so, else throws an error message.

## `toString`

_Available since version 0.10.0._

Converts the given argument to a string.

## `codepoint`

_Available since version 0.10.0._

Returns the positive integer representing the unicode codepoint of the character in the given single-character string. This function is the inverse of `char`.

## `char`

_Available since version 0.10.0._

Returns a string of length one whose only unicode codepoint has integer id n. This function is the inverse of `std.codepoint`.

## `substr`

_Available since version 0.10.0._

`std.substr(str, from, len)` returns a string that is the part of `s` that starts at `offset` from and is `len` codepoints long.

If the string `s` is shorter than `from + len`, the suffix starting at position `from` will be returned.

## `findSubstr`

_Available since version 0.10.0._

`std.findSubstr(pat, str)` returns an array that contains the indexes of all occurrences of pat in str.

## `startsWith`

_Available since version 0.10.0._

`std.startsWith(a, b)` returns whether the string `a` is prefixed by the string `b`.

## `endsWith`

_Available since version 0.10.0._

`std.endsWith(a, b)` returns whether the string `a` is suffixed by the string `b`.

## `stripChars`

_Available since version 0.15.0._

`std.stripChars(str, chars)` removes characters `chars` from the beginning and from the end of `str`.

Examples:

```jsonnet
assert std.stripChars(" test test test ", " ") == "test test test";
assert std.stripChars("aaabbbbcccc", "ac") == "bbbb";
assert std.stripChars("cacabbbbaacc", "ac") == "bbbb";
```

## `lstripChars`

_Available since version 0.15.0._

`std.lstripChars(str, chars)` removes characters `chars` from the beginning of `str`.

Examples:

```jsonnet
assert std.lstripChars(" test test test ", " ") == "test test test ";
assert std.lstripChars("aaabbbbcccc", "ac") == "bbbbcccc";
assert std.lstripChars("cacabbbbaacc", "ac") == "bbbbaacc";
```

## `rstripChars`

_Available since version 0.15.0._

`std.rstripChars(str, chars)` removes characters `chars` from the end of `str`.

```jsonnet
assert std.rstripChars(" test test test ", " ") == " test test test";
assert std.rstripChars("aaabbbbcccc", "ac") == "aaabbbb";
assert std.rstripChars("cacabbbbaacc", "ac") == "cacabbbb";
```

## `split`

_Available since version 0.10.0._

`std.split(str, c)` splits the string `str` into an array of strings, divided by the string `c`.

Note: Versions up to and including 0.18.0 require `c` to be a single character.

Examples:

```jsonnet
assert std.split("foo/_bar", "/_") == [ "foo", "bar" ];
assert std.split("/_foo/\_bar", "/_") == [ "", "foo", "bar" ];
```

## `splitLimit`

_Available since version 0.10.0._

`std.splitLimit(str, c, maxsplits)` is the same as `std.split(str, c)` but will stop after `maxsplits` splits, thereby the largest array it will return has length `maxsplits + 1`. A limit of `-1` means unlimited.

Note: Versions up to and including 0.18.0 require `c` to be a single character.

Examples:

```jsonnet
assert splitLimit("foo/_bar", "/_", 1) == [ "foo", "bar" ];
assert splitLimit("/_foo/\_bar", "/_", 1) == [ "", "foo/_bar" ];
```

## `splitLimitR`

_Available since version 0.19.0._

`std.splitLimitR(str, c, maxsplits)` is the same as `std.splitLimit(str, c, maxsplits)` but will split from right to left.

Example:

```jsonnet
assert splitLimitR("/_foo/\_bar", "/_", 1) == [ "/_foo", "bar" ];
```

## `strReplace`

_Available since version 0.10.0._

`std.strReplace(str, from, to)` returns a copy of the string `str` in which all occurrences of string `from` have been replaced with string `to`.

Example:

```jsonnet
assert std.strReplace('I like to skate with my skateboard', 'skate', 'surf')
  == "I like to surf with my surfboard";
```

## `isEmpty`

_Available since version 0.20.0._

Returns `true` if the given string is of zero length.

## `asciiUpper`

_Available since version 0.10.0._

Returns a copy of the string in which all ASCII letters are capitalized.

Example:

```jsonnet
assert std.asciiUpper('100 Cats!') == "100 CATS!";
```

## `asciiLower`

_Available since version 0.10.0._

Returns a copy of the string in which all ASCII letters are lower cased.

Example:

```jsonnet
assert std.asciiLower('100 Cats!') == "100 cats!";
```

## `stringChars`

_Available since version 0.10.0._

Split the string into an array of strings, each containing a single codepoint.

Example:

```jsonnet
assert std.stringChars("foo") == [ "f", "o", "o" ];
```

## `format`

_Available since version 0.10.0._

`std.format(str, vals)` format the string `str` using the values in `vals`.

The `vals` can be an array, an object, or in other cases are treated as if they were provided in a singleton array.

The string formatting follows the same rules as Python.

The `%` operator can be used as a shorthand for this function.

Examples:

```jsonnet
assert std.format("Hello %03d", 12) == "Hello 012";
assert "Hello %03d" % 12 == "Hello 012";
assert "Hello %s, age %d" % ["Foo", 25] == "Hello Foo, age 25";
assert "Hello %(name)s, age %(age)d" % {age: 25, name: "Foo"}
  == "Hello Foo, age 25";
```

## `escapeStringBash`

_Available since version 0.10.0._

Wraps the string in single quotes, and escapes any single quotes within `str` by changing them to a sequence `'"'"'`.

This allows injection of arbitrary strings as arguments of commands in bash scripts.

## `escapeStringDollars`

_Available since version 0.10.0._

Converts `$` to `$$` in the string.

This allows injection of arbitrary strings into systems that use `$` for string interpolation, like Terraform.

## `escapeStringJson`

_Available since version 0.10.0._

Converts the string to allow it to be embedded in a JSON representation, within a string.

This adds quotes, escapes backslashes, and escapes unprintable characters.

Example:

```jsonnet
assert "{name: %s}" % std.escapeStringJson("Multiline\nc:\\path")
  == "{name: \"Multiline\\nc:\\\\path\"}";
```

## `escapeStringPython`

_Available since version 0.10.0._

Converts the string to allow it to be embedded in Python.

This is an alias for `escapeStringJson`.

## `escapeStringXml`

_Available since version 0.10.0._

Converts the string to allow it to be embedded in XML (or HTML). The following replacements are made:

| Replace | With     |
| ------- | -------- |
| `<`     | `&lt;`   |
| `>`     | `&gt;`   |
| `&`     | `&amp;`  |
| `"`     | `&quot;` |
| `'`     | `&apos;` |

## `parseInt`

_Available since version 0.10.0._

Parses a signed decimal integer from the input string.

Examples:

```jsonnet
assert std.parseInt("123") == 123;
assert std.parseInt("-123") == -123;
```

## `parseOctal`

_Available since version 0.10.0._

Parses an unsigned octal integer from the input string. Initial zeroes are tolerated.

Example:

```jsonnet
assert std.parseOctal("755") == 493;
```

## `parseHex`

_Available since version 0.10.0._

Parses an unsigned hexadecimal integer, from the input string. Case insensitive.

Example:

```jsonnet
assert std.parseHex("ff") == 255;
```

## `parseJson`

_Available since version 0.13.0._

Parses a JSON string.

Example:

```jsonnet
assert std.parseJson('{"foo": "bar"}') == { "foo": "bar" };
```

## `parseYaml`

_Available since version 0.18.0._

Parses a YAML string.

This is provided as a "best-effort" mechanism and should not be relied on to provide a fully standards compliant YAML parser.

YAML is a superset of JSON, consequently "downcasting" or manifestation of YAML into JSON or Jsonnet values will only succeed when using the subset of YAML that is compatible with JSON.

The parser does not support YAML documents with scalar values at the root. The root node of a YAML document must start with either a YAML sequence or map to be successfully parsed.

Example:

```jsonnet
assert std.parseYaml('foo: bar') == { "foo": "bar" };
```

## `encodeUTF8`

_Available since version 0.13.0._

Encode a string using UTF8. Returns an array of numbers representing bytes.

## `decodeUTF8`

_Available since version 0.13.0._

Decode an array of numbers representing bytes using UTF8. Returns a string.

## `manifestIni`

_Available since version 0.10.0._

Convert the given structure to a string in INI format.

This allows using Jsonnet's object model to build a configuration to be consumed by an application expecting an INI file. The data is in the form of a set of sections, each containing a key/value mapping.

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

## `manifestPython`

_Available since version 0.10.0._

Convert the given value to a JSON-like form that is compatible with Python. The chief differences are:

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

## `manifestPythonVars`

_Available since version 0.10.0._

Convert the given object to a JSON-like form that is compatible with Python. The key difference to `std.manifestPython` is that the top level is represented as a list of Python global variables.

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

## `manifestJsonEx`

_Available since version 0.10.0._

`std.manifestJsonEx(value, indent, newline, key_val_sep)` convert the given object to a JSON form.

`indent` is a string containing one or more whitespace characters that are used for indentation.

`newline` is by default `"\n"` and is inserted where a newline would normally be used to break long lines.

`key_val_sep` is used to separate the key and value of an object field:

## `manifestJson`

_Available since version 0.10.0._

Convert the given object to a JSON form.

Under the covers, it calls `std.manifestJsonEx` with a 4-space indent.

## `manifestJsonMinified`

_Available since version 0.18.0._

Convert the given object to a minified JSON form, with no extra whitespace.

Under the covers, it calls `std.manifestJsonEx`.

## `manifestYamlDoc`

_Available since version 0.10.0._

`std.manifestYamlDoc(value, indent_array_in_object=false, quote_keys=true)` convert the given value to a YAML form.

Note that `manifestJson` could also be used for this purpose, because any JSON is also valid YAML. But this function will produce more canonical-looking YAML.

The `indent_array_in_object` parameter adds additional indentation which some people may find easier to read.

The `quote_keys` parameter controls whether YAML identifiers are always quoted or only when necessary.

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

## `manifestYamlStream`

_Available since version 0.10.0._

Given an array of values, `std.manifestYamlStream(value, indent_array_in_object=false, c_document_end=false, quote_keys=true)` emits a YAML "stream", which is a sequence of documents separated by `---` and ending with `...`.

The `indent_array_in_object` and `quote_keys` params are the same as in `std.manifestYamlDoc`.

The `c_document_end` param adds the optional terminating `...`.

This example:

```jsonnet
std.manifestYamlStream( ['a', 1, []], indent_array_in_object=false, c_document_end=true)
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

## `manifestXmlJsonml`

_Available since version 0.10.0._

Convert the given [JsonML](http://www.jsonml.org)-encoded value to a string containing the XML.

This example:

```jsonnet
std.manifestXmlJsonml([
  'svg', { height: 100, width: 100 },
  [
    'circle', {
      cx: 50, cy: 50, r: 40,
      stroke: 'black', 'stroke-width': 3,
      fill: 'red',
    }
  ],
])
```

Yields a string containing this XML (all on one line):

```xml
<svg height="100" width="100">
    <circle cx="50" cy="50" fill="red" r="40"
    stroke="black" stroke-width="3"></circle>;
</svg>;

```

JsonML is designed to preserve "mixed-mode content" (i.e., textual data outside of or next to elements). This includes the whitespace needed to avoid having all the XML on one line, which is meaningful in XML. In order to have whitespace in the XML output, it must be present in the JsonML input:

```jsonnet
std.manifestXmlJsonml([
  'svg',
  { height: 100, width: 100 },
  '\n  ',
  [
    'circle', {
      cx: 50, cy: 50, r: 40, stroke: 'black',
      'stroke-width': 3, fill: 'red',
    }
  ],
  '\n',
])
```

## `manifestTomlEx`

`std.manifestTomlEx(toml, indent)` convert the given `toml` to a TOML form.

`indent` is a string containing one or more whitespace characters that are used for indentation.

## `makeArray`

_Available since version 0.10.0._

`std.makeArray(sz, func)` creates a new array of `sz` elements by calling `func` to initialize each element.

`func` is a function that takes a single parameter, the index of the element it should initialize.

Example:

```jsonnet
assert std.makeArray(3,function(x) x * x) == [ 0, 1, 4 ];
```

## `member`

_Available since version 0.15.0._

`std.member(arr, x)` returns whether `x` occurs in `arr`.

Argument `arr` may be an array or a string.

## `count`

_Available since version 0.10.0._

`std.count(arr, x)` returns the number of times that `x` occurs in `arr`.

## `find`

_Available since version 0.10.0._

`std.find(value, arr)` returns an array that contains the indexes of all occurrences of `value` in `arr`.

## `map`

_Available since version 0.10.0._

`std.map(func, arr)` applies the given `func` to every element of `arr` to form a new array.

## `mapWithIndex`

_Available since version 0.10.0._

Similar to `std.map`, but it also passes to the function the element's index in the array. The function is expected to take the index as the first parameter and the element as the second.

## `filterMap`

_Available since version 0.10.0._

`std.filterMap(filter_func, map_func, arr)` first filters with `filter_func`, then maps with `map_func`, the given array `arr`.

## `flatMap`

_Available since version 0.10.0._

`std.flatMap(func, arr)` applies the given function to every element of `arr` to form a new array then flatten the result.

The argument `arr` must be an array or a string.

- If `arr` is an array, `func` must return an array.
- If `arr` is a string, `func` must return a string.

`std.flatMap` can be thought of as a generalized `map`, with each element mapped to 0, 1 or more elements.

Examples:

```jsonnet
assert std.flatMap(function(x) [x, x], [1, 2, 3])
  == [ 1, 1, 2, 2, 3, 3 ];
assert std.flatMap(function(x) if x == 2 then [] else [x], [1, 2, 3])
  == [ 1, 3 ];
assert std.flatMap(function(x) if x == 2 then [] else [x * 3, x * 2], [1, 2, 3])
  == [ 3, 2, 9, 6 ];
assert std.flatMap(function(x) x+x, "foo")
  == "ffoooo";
```

## `filter`

_Available since version 0.10.0._

`std.filter(func, arr)` return a new array containing all the elements of `arr` for which the `func` function returns `true`.

## `foldl`

_Available since version 0.10.0._

`std.foldl(func, arr, init)` calls the function `func` on the result of the previous function call and each array element of `arr`, or `init` in the case of the initial element. Traverses `arr` from left to right.

Example:

```jsonnet
local cmb(ac, x) = "(%s %s)" % [ac, x];
assert std.foldl(cmb, ["a", "b", "c"], "_")
  == "(((_ a) b) c)";
```

## `foldr`

_Available since version 0.10.0._

`std.foldr(func, arr, init)` calls the function `func` on the result of the previous function call and each array element of `arr`, or `init` in the case of the initial element. Traverses `arr` from right to left.

Example:

```jsonnet
local cmb(ac, x) = "(%s %s)" % [ac, x];
assert std.foldr(cmb, ["a", "b", "c"], "_")
  == "(((_ c) b) a)";
```

## `range`

_Available since version 0.10.0._

`std.range(from, to)` returns an array of ascending numbers between `from` and `to`, inclusively.

Example:

```jsonnet
assert std.range(2, 6) == [2, 3, 4, 5, 6];
```

## `repeat`

_Available since version 0.15.0._

`std.repeat(what, count)` repeats an array or a string `what` a number of times specified by an integer `count`.

Examples:

```jsonnet
assert std.repeat([1, 2, 3], 3)
  == [ 1, 2, 3, 1, 2, 3, 1, 2, 3 ];
assert std.repeat("blah", 2)
  == "blahblah";
```

## `slice`

_Available since version 0.10.0._

`std.slice(indexable, index, end, step)` selects the elements of `indexable`, an array or a string, from `index` to `end` with `step`, and returns an array or a string respectively.

Note that it's recommended to use dedicated slicing syntax both for arrays and strings (e.g. `arr[0:4:1]` instead of `slice(arr, 0, 4, 1)`).

Examples:

```jsonnet
assert std.slice([1, 2, 3, 4, 5, 6], 0, 4, 1) == [ 1, 2, 3, 4 ];
assert std.slice([1, 2, 3, 4, 5, 6], 1, 6, 2) == [ 2, 4, 6 ];
assert std.slice("jsonnet", 0, 4, 1) == "json";
assert std.slice("jsonnet", -3, null, null) == "net";
```

## `join`

_Available since version 0.10.0._

For `std.join(sep, arr)`, if `sep` is a string, then `arr` must be an array of strings, in which case they are concatenated with `sep` used as a delimiter.

If `sep` is an array, then `arr` must be an array of arrays, in which case the arrays are concatenated in the same way, to produce a single array.

Examples:

```jsonnet
assert std.join(".", ["www", "google", "com"]) == "www.google.com";
assert std.join([9, 9], [[1], [2, 3]]) == [ 1, 9, 9, 2, 3 ];
```

## `lines`

_Available since version 0.10.0._

Concatenate an array of strings into a text file with newline characters after each string. This is suitable for constructing bash scripts and the like.

## `flattenArrays`

_Available since version 0.10.0._

Concatenate an array of arrays into a single array.

Example:

```jsonnet
assert std.flattenArrays([[1, 2], [3, 4], [[5, 6], [7, 8]]])
  == [ 1, 2, 3, 4, [ 5, 6 ], [ 7, 8 ] ];
```

## `reverse`

_Available since version 0.13.0._

Returns the argument array reversed.

Examples:

```jsonnet
assert std.reverse([2, 4, 6]) == [6, 4, 2];
assert std.reverse([8]) == [8];
assert std.reverse([]) == [];
```

## `sort`

_Available since version 0.10.0._

`std.sort(arr, keyF=id)` sorts the array using the `<=` operator.

The optional argument `keyF` is a single argument function used to extract comparison key from each array element.

Examples:

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
]
```

## `uniq`

_Available since version 0.10.0._

`std.uniq(arr, keyF=id)` removes successive duplicates. When given a sorted array, removes all duplicates.

The optional argument `keyF` is a single argument function used to extract comparison key from each array element.

Examples:

```jsonnet
assert std.uniq([1, 1, 1]) == [1];
assert std.uniq([1, 2, 2, 3, 2]) == [1, 2, 3, 2];
```

## `all`

_Available since version 0.19.0._

`std.all(arr)` returns `true` if all elements of the input array are `true`, `false` otherwise. `std.all([])` evaluates to `true`.

Raises if `arr` is not an array, or `arr` contains non-boolean values.

## `any`

_Available since version 0.19.0._

`std.any(arr)` return `true` if any element of `arr` is `true`, `false` otherwise. `std.any([])` evaluates to `false`.

Raises if `arr` is not an array, or `arr` contains non-boolean values.

## `sum`

_Available since version 0.20.0._

Returns the sum of all the elements.

## `avg`

_Available since version 0.20.0._

Returns the average of all the elements.

## `set`

_Available since version 0.10.0._

`std.set(arr, keyF=id)` is a shortcut for `std.uniq(std.sort(arr))`.

## `setInter`

_Available since version 0.10.0._

`std.setInter(a, b, keyF=id)` is the set intersection operation (values in both `a` and `b`).

`a` and `b` must be sets, i.e. sorted arrays with no duplicates. If that is not the case, this function will quietly return non-meaningful results.

The optional `keyF` function can be used to extract a key to use from each element. This key is used for the purpose of identifying uniqueness.

## `setUnion`

_Available since version 0.10.0._

`std.setUnion(a, b, keyF=id)` is the set union operation (values in any of `a` or `b`).

Note that `+` on sets will simply concatenate the arrays, possibly forming an array that is not a set (due to not being ordered without duplicates).

Examples:

```jsonnet
assert std.setUnion([1, 2], [2, 3]) == [ 1, 2, 3 ];
assert std.setUnion(
  [{n:"A", v:1}, {n:"B"}],
  [{n:"A", v: 9999}, {n:"C"}],
  keyF=function(x) x.n
) == [ { "n": "A", "v": 1 }, { "n": "B" }, { "n": "C" } ];
```

`a` and `b` must be sets, i.e. sorted arrays with no duplicates. If that is not the case, this function will quietly return non-meaningful results.

The optional `keyF` function can be used to extract a key to use from each element. This key is used for the purpose of identifying uniqueness.

## `setDiff`

_Available since version 0.10.0._

`std.setDiff(a, b, keyF=id)` is the set difference operation (values in `a` but not `b`).

`a` and `b` must be sets, i.e. sorted arrays with no duplicates. If that is not the case, this function will quietly return non-meaningful results.

The optional `keyF` function can be used to extract a key to use from each element. This key is used for the purpose of identifying uniqueness.

## `setMember`

_Available since version 0.10.0._

`std.setMember(x, s, keyF=id)` returns `true` if `x` is a member of `s`, otherwise `false`.

`s` must be a set, i.e. a sorted array with no duplicates. If that is not the case, this function will quietly return non-meaningful results.

The optional `keyF` function can be used to extract a key to use from each element. This key is used for the purpose of identifying uniqueness.

## `get`

_Available since version 0.18.0._

`std.get(o, f, default=null, inc_hidden=true)` returns the object `o`'s field `f` if it exists or `default` value otherwise. `inc_hidden` controls whether to include hidden fields.

## `objectHas`

_Available since version 0.10.0._

`std.objectHas(o, f)` returns `true` if the given object `o` has the field `f` (given as a string), otherwise `false`.

Raises an error if the arguments are not object and string respectively.

Returns `false` if the field is hidden.

## `objectFields`

_Available since version 0.10.0._

Returns an array of strings, each element being a field from the given object.

**Does not** include hidden fields.

## `objectValues`

_Available since version 0.17.0._

Returns an array of the values in the given object.

**Does not** include hidden fields.

## `objectKeysValues`

_Available since version 0.20.0._

Returns an array of objects from the given object, each object having two fields: key (string) and value (object).

**Does not** include hidden fields.

## `objectHasAll`

_Available since version 0.10.0._

Like `std.objectHas` but also includes hidden fields.

## `objectFieldsAll`

_Available since version 0.10.0._

Like `std.objectFields` but also includes hidden fields.

## `objectValuesAll`

_Available since version 0.17.0._

Like `std.objectValues` but also includes hidden fields.

## `objectKeysValuesAll`

_Available since version 0.20.0._

Like `std.objectKeysValues` but also includes hidden fields.

## `mapWithKey`

_Available since version 0.10.0._

`std.mapWithKey(func, obj)` applies the given `func` to all fields of the given `obj`, also passing the field name.

The function `func` is expected to take the field name as the first parameter and the field value as the second.

## `base64`

_Available since version 0.10.0._

Encodes the given value into a base64 string.

The encoding sequence is `A-Za-z0-9+/` with `=` to pad the output to a multiple of 4 characters.

The value can be a string or an array of numbers, but the codepoints / numbers must be in the 0 to 255 range.

The resulting string has no line breaks.

## `base64DecodeBytes`

_Available since version 0.10.0._

`std.base64DecodeBytes(str)` decodes the given base64 string into an array of bytes (number values).

Currently assumes the input string has no line breaks and is padded to a multiple of 4 (with the `=` character). In other words, it consumes the output of `base64`.

## `base64Decode`

_Available since version 0.10.0._

**Deprecated**: use `std.base64DecodeBytes` and decode the string explicitly (e.g. with `std.decodeUTF8`) instead.

Behaves like `std.base64DecodeBytes` except returns a naively encoded string instead of an array of bytes.

## `md5`

_Available since version 0.10.0._

Encodes the given value into an MD5 string.

## `xor`

_Available since version 0.20.0._

Returns the xor (exclusive or) of the two given booleans.

## `xnor`

_Available since version 0.20.0._

Returns the xnor (exclusive nor) of the two given booleans.

## `mergePatch`

_Available since version 0.10.0._

`std.mergePatch(target, patch)` applies `patch` to `target` according to [RFC7396](https://tools.ietf.org/html/rfc7396).

## `trace`

_Available since version 0.11.0._

`std.trace(str, rest)` outputs the given string `str` to stderr and returns `rest` as the result.

Example:

```jsonnet
local choose(c, yes, no) =
  if c then
    std.trace('c is true, returning ' + std.toString(yes), yes)
  else
    std.trace('c is false, returning ' + std.toString(no), no);

{
  foo: choose(true, { bar: 1 }, { quz: 2 }),
}
```

Prints:

```
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

## `equals`

Returns whether the two arguments equal each other.

## `objectHasEx`

TODO
