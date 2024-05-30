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

| type     | something  |
| -------- | ---------- |
| array    | elements   |
| string   | codepoints |
| function | parameters |
| object   | fields     |

Raises an error if given a primitive value, i.e. `null`, `true` or `false`.

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

Returns -1, 0, or 1 if the number is negative, zero, or positive.

## `max`

Returns the maximum of the two arguments.

## `min`

Returns the minimum of the two arguments.

## `pow`

`pow(x, y)` returns $x^y$, i.e. $x$ to the $y$ power.

## `exp`

`exp(x)` returns $e^x$, i.e. $e$ (2.71828...) to the $x$ power.

## `log`

`log(x)` returns the natural logarithm of $x$, i.e. the solution $y$ in $e^y = x$.

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

This is a trig function.

## `cos`

This is a trig function.

## `tan`

This is a trig function.

## `asin`

This is a trig function.

## `acos`

This is a trig function.

## `atan`

This is a trig function.

## `round`

This is a trig function.

## `isEven`

Returns `true` if this is an even number, else `false`.

## `isOdd`

Returns `true` if this is an odd number, else `false`.

## `isInteger`

Returns `true` if this is an integer number, else `false`.

## `isDecimal`

Returns TODO

## `mod`

This is what the `%` operator is desugared to. It performs modulo arithmetic if the left hand side is a number, or if the left hand side is a string, it does Python-style string formatting with `format`.

## `clamp`

_Available since version 0.15.0._

`clamp(x, minVal, maxVal)` clamps a value to fit within the range `[minVal,maxVal]`.

Equivalent to `max(minVal, min(x, maxVal))`.

Examples:

- `clamp(-3, 0, 5) == 0`
- `clamp(4, 0, 5) == 4`
- `clamp(7, 0, 5) == 5`

## `assertEqual`

_Available since version 0.10.0._

`assertEqual(a, b)` ensures that a == b. Returns true or throws an error message.

## `toString`

_Available since version 0.10.0._

Converts the given argument to a string.

## `codepoint`

_Available since version 0.10.0._

Returns the positive integer representing the unicode codepoint of the character in the given single-character string. This function is the inverse of `char`.

## `char`

_Available since version 0.10.0._

Returns a string of length one whose only unicode codepoint has integer id n. This function is the inverse of `codepoint`.

## `substr`

_Available since version 0.10.0._

`substr(str, from, len)` returns a string that is the part of `s` that starts at `offset` from and is `len` codepoints long.

If the string `s` is shorter than `from + len`, the suffix starting at position `from` will be returned.

## `findSubstr`

_Available since version 0.10.0._

`findSubstr(pat, str)` returns an array that contains the indexes of all occurrences of pat in str.

## `startsWith`

_Available since version 0.10.0._

`startsWith(a, b)` returns whether the string `a` is prefixed by the string `b`.

## `endsWith`

_Available since version 0.10.0._

`endsWith(a, b)` returns whether the string `a` is suffixed by the string `b`.

## `stripChars`

_Available since version 0.15.0._

`stripChars(str, chars)` removes characters `chars` from the beginning and from the end of `str`.

Examples:

- `stripChars(" test test test ", " ") == "test test test"`
- `stripChars("aaabbbbcccc", "ac") == "bbbb"`
- `stripChars("cacabbbbaacc", "ac") == "bbbb"`

## `lstripChars`

_Available since version 0.15.0._

`lstripChars(str, chars)` removes characters `chars` from the beginning of `str`.

Examples:

- `lstripChars(" test test test ", " ") == "test test test "`
- `lstripChars("aaabbbbcccc", "ac") == "bbbbcccc"`
- `lstripChars("cacabbbbaacc", "ac") == "bbbbaacc"`

## rstripChars

_Available since version 0.15.0._

`rstripChars(str, chars)` removes characters `chars` from the end of `str`.

- `rstripChars(" test test test ", " ") == " test test test"`
- `rstripChars("aaabbbbcccc", "ac") == "aaabbbb"`
- `rstripChars("cacabbbbaacc", "ac") == "cacabbbb"`

## `split`

_Available since version 0.10.0._

`split(str, c)` splits the string `str` into an array of strings, divided by the string `c`.

Note: Versions up to and including 0.18.0 require `c` to be a single character.

Examples:

- `split("foo/_bar", "/_") == [ "foo", "bar" ]`
- `split("/_foo/\_bar", "/_") == [ "", "foo", "bar" ]`

## `splitLimit`

_Available since version 0.10.0._

`splitLimit(str, c, maxsplits)` is the same as `split(str, c)` but will stop after `maxsplits` splits, thereby the largest array it will return has length `maxsplits + 1`. A limit of `-1` means unlimited.

Note: Versions up to and including 0.18.0 require `c` to be a single character.

Examples:

- `splitLimit("foo/_bar", "/_", 1) == [ "foo", "bar" ]`
- `splitLimit("/_foo/\_bar", "/_", 1) == [ "", "foo/_bar" ]`

## `splitLimitR`

_Available since version 0.19.0._

`splitLimitR(str, c, maxsplits)` is the same as `std.splitLimit(str, c, maxsplits)` but will split from right to left.

Example: `std.splitLimitR("/_foo/\_bar", "/_", 1) == [ "/_foo", "bar" ]`

## `strReplace`

_Available since version 0.10.0._

`strReplace(str, from, to)` returns a copy of the string `str` in which all occurrences of string `from` have been replaced with string `to`.

Example: `strReplace('I like to skate with my skateboard', 'skate', 'surf') == "I like to surf with my surfboard"`

## `isEmpty`

_Available since version 0.20.0._

Returns true if the given string is of zero length.

## `asciiUpper`

_Available since version 0.10.0._

Returns a copy of the string in which all ASCII letters are capitalized.

Example: `asciiUpper('100 Cats!') == "100 CATS!"`

## `asciiLower`

_Available since version 0.10.0._

Returns a copy of the string in which all ASCII letters are lower cased.

Example: `asciiLower('100 Cats!') == "100 cats!"`

## `stringChars`

_Available since version 0.10.0._

Split the string into an array of strings, each containing a single codepoint.

Example: `std.stringChars("foo") == [ "f", "o", "o" ]`

## `format`

_Available since version 0.10.0._

`std.format(str, vals)` format the string `str` using the values in `vals`.

The `vals` can be an array, an object, or in other cases are treated as if they were provided in a singleton array.

The string formatting follows the same rules as Python.

The `%` operator can be used as a shorthand for this function.

Examples:

- `std.format("Hello %03d", 12) yields "Hello 012"`
- `"Hello %03d" % 12 == "Hello 012"`
- `"Hello %s, age %d" % ["Foo", 25] == "Hello Foo, age 25"`
- `"Hello %(name)s, age %(age)d" % {age: 25, name: "Foo"} == "Hello Foo, age 25"`

## `escapeStringBash`

_Available since version 0.10.0._

Wraps the string in single quotes, and escapes any single quotes within `str` by changing them to a sequence `'"'"'`. This allows injection of arbitrary strings as arguments of commands in bash scripts.

## `escapeStringDollars`

_Available since version 0.10.0._

Converts `$` to `$$` in the string. This allows injection of arbitrary strings into systems that use `$` for string interpolation (like Terraform).

## `escapeStringJson`

_Available since version 0.10.0._

Converts the string to allow it to be embedded in a JSON representation, within a string. This adds quotes, escapes backslashes, and escapes unprintable characters.

Example:

```jsonnet
local description = "Multiline\nc:\\path";
"{name: %s}" % std.escapeStringJson(description)
// "{name: \"Multiline\\nc:\\\\path\"}"
```

## `escapeStringPython`

_Available since version 0.10.0._

Converts the string to allow it to be embedded in Python. This is an alias for `escapeStringJson`.

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

- `parseInt("123") == 123`
- `parseInt("-123") == -123`

## `parseOctal`

_Available since version 0.10.0._

Parses an unsigned octal integer from the input string. Initial zeroes are tolerated.

Example: `std.parseOctal("755") == 493`

## `parseHex`

_Available since version 0.10.0._

Parses an unsigned hexadecimal integer, from the input string. Case insensitive.

Example: `std.parseHex("ff") == 255`

## `parseJson`

_Available since version 0.13.0._

Parses a JSON string.

Example: `std.parseJson('{"foo": "bar"}') == { "foo": "bar" }`

## `parseYaml`

_Available since version 0.18.0._

Parses a YAML string.

This is provided as a "best-effort" mechanism and should not be relied on to provide a fully standards compliant YAML parser.

YAML is a superset of JSON, consequently "downcasting" or manifestation of YAML into JSON or Jsonnet values will only succeed when using the subset of YAML that is compatible with JSON.

The parser does not support YAML documents with scalar values at the root. The root node of a YAML document must start with either a YAML sequence or map to be successfully parsed.

Example: `std.parseYaml('foo: bar') == { "foo": "bar" }`

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

These examples should make it clear:

```jsonnet
{
  main: { a: "1", b: "2" },
  sections: {
    s1: { x: "11", y: "22", z: "33" },
    s2: { p: "yes", q: ""},
    empty: {},
  }
}
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

Convert the given value to a JSON-like form that is compatible with Python. The chief differences are `True` / `False` / `None` instead of `true` / `false` / `null`.

```jsonnet
{
  b: ["foo", "bar"],
  c: true,
  d: null,
  e: { f1: false, f2: 42 },
}
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

Convert the given object to a JSON-like form that is compatible with Python. The key difference to `manifestPython` is that the top level is represented as a list of Python global variables.

```jsonnet
{
  b: ["foo", "bar"],
  c: true,
  d: null,
  e: { f1: false, f2: 42 },
}
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

`manifestJsonEx(value, indent, newline, key_val_sep)` convert the given object to a JSON form.

`indent` is a string containing one or more whitespaces that are used for indentation.

`newline` is by default `"\n"` and is inserted where a newline would normally be used to break long lines.

`key_val_sep` is used to separate the key and value of an object field:

## `manifestJson`

_Available since version 0.10.0._

Convert the given object to a JSON form. Under the covers, it calls `manifestJsonEx` with a 4-space indent.

## `manifestJsonMinified`

_Available since version 0.18.0._

Convert the given object to a minified JSON form. Under the covers, it calls `manifestJsonEx`.

## `manifestYamlDoc`

_Available since version 0.10.0._

`manifestYamlDoc(value, indent_array_in_object=false, quote_keys=true)` convert the given value to a YAML form. Note that `manifestJson` could also be used for this purpose, because any JSON is also valid YAML. But this function will produce more canonical-looking YAML.

```jsonnet
manifestYamlDoc(
  {
      x: [1, 2, 3, true, false, null,
          "string\nstring\n"],
      y: { a: 1, b: 2, c: [1, 2] },
  },
  indent_array_in_object=false)
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

The `indent_array_in_object` param adds additional indentation which some people may find easier to read.

The `quote_keys` parameter controls whether YAML identifiers are always quoted or only when necessary.

## `manifestYamlStream`

_Available since version 0.10.0._

Given an array of values, `manifestYamlStream(value, indent_array_in_object=false, c_document_end=false, quote_keys=true)` emits a YAML "stream", which is a sequence of documents separated by `---` and ending with `...`.

```jsonnet
manifestYamlStream( ['a', 1, []], indent_array_in_object=false, c_document_end=true)
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

The `indent_array_in_object` and `quote_keys` params are the same as in `manifestYamlDoc`.

The `c_document_end` param adds the optional terminating `...`.

## `manifestXmlJsonml`

_Available since version 0.10.0._

Convert the given [JsonML](http://www.jsonml.org)-encoded value to a string containing the XML.

```jsonnet
manifestXmlJsonml([
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
manifestXmlJsonml([
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

`std.manifestTomlEx(toml, indent)` convert the given object to a TOML form. `indent` is a string containing one or more whitespaces that are used for indentation.

## `makeArray`

_Available since version 0.10.0._

`makeArray(sz, func)` creates a new array of `sz` elements by calling `func(i)` to initialize each element. `func` is a function that takes a single parameter, the index of the element it should initialize.

Example: `std.makeArray(3,function(x) x * x) == [ 0, 1, 4 ]`

## `member`

_Available since version 0.15.0._

`member(arr, x)` returns whether `x` occurs in `arr`.

Argument `arr` may be an array or a string.

## `count`

_Available since version 0.10.0._

`count(arr, x)` returns the number of times that x occurs in arr.

## `find`

_Available since version 0.10.0._

`find(value, arr)` returns an array that contains the indexes of all occurrences of `value` in `arr`.

## `map`

_Available since version 0.10.0._

`map(func, arr)` applies the given `func` to every element of `arr` to form a new array.

## `mapWithIndex`

_Available since version 0.10.0._

Similar to map above, but it also passes to the function the element's index in the array. The function is expected to take the index as the first parameter and the element as the second.

## `filterMap`

_Available since version 0.10.0._

`filterMap(filter_func, map_func, arr)` first filters, then maps the given array, using the two functions provided.

## `flatMap`

_Available since version 0.10.0._

`flatMap(func, arr)` applies the given function to every element of `arr` to form a new array then flatten the result. The argument `arr` must be an array or a string. If `arr` is an array, `func` must return an array. If `arr` is a string, `func` must return an `string`.

`flatMap` can be thought of as a generalized `map`, with each element mapped to 0, 1 or more elements.

- `flatMap(function(x) [x, x], [1, 2, 3]) == [ 1, 1, 2, 2, 3, 3 ]`
- `flatMap(function(x) if x == 2 then [] else [x], [1, 2, 3]) == [ 1, 3 ]`
- `flatMap(function(x) if x == 2 then [] else [x * 3, x * 2], [1, 2, 3]) == [ 3, 2, 9, 6 ]`
- `flatMap(function(x) x+x, "foo") == "ffoooo"`

## `filter`

_Available since version 0.10.0._

`filter(func, arr)` return a new array containing all the elements of `arr` for which the `func` function returns `true`.

## `foldl`

_Available since version 0.10.0._

`foldl(func, arr, init)` calls the function `func` on the result of the previous function call and each array element of `arr`, or `init` in the case of the initial element. Traverses `arr` from left to right.

## `foldr`

_Available since version 0.10.0._

`foldr(func, arr, init)` calls the function `func` on the result of the previous function call and each array element of `arr`, or `init` in the case of the initial element. Traverses `arr` from right to left.

## `range`

_Available since version 0.10.0._

`range(from, to)` returns an array of ascending numbers between `from` and `to`, inclusively.

## `repeat`

_Available since version 0.15.0._

`repeat(what, count)` repeats an array or a string `what` a number of times specified by an integer `count`.

Examples:

- `repeat([1, 2, 3], 3) == [ 1, 2, 3, 1, 2, 3, 1, 2, 3 ]`
- `repeat("blah", 2) == "blahblah"`

## `slice`

_Available since version 0.10.0._

`slice(indexable, index, end, step)` selects the elements of `indexable`, an array or a string, from `index` to `end` with `step`, and returns an array or a string respectively.

Note that it's recommended to use dedicated slicing syntax both for arrays and strings (e.g. `arr[0:4:1]` instead of `slice(arr, 0, 4, 1)`).

Examples:

- `slice([1, 2, 3, 4, 5, 6], 0, 4, 1) == [ 1, 2, 3, 4 ]`
- `slice([1, 2, 3, 4, 5, 6], 1, 6, 2) == [ 2, 4, 6 ]`
- `slice("jsonnet", 0, 4, 1) == "json"`
- `slice("jsonnet", -3, null, null) == "net"`

## `join`

_Available since version 0.10.0._

For `join(sep, arr)`, if `sep` is a string, then `arr` must be an array of strings, in which case they are concatenated with `sep` used as a delimiter.

If `sep` is an array, then `arr` must be an array of arrays, in which case the arrays are concatenated in the same way, to produce a single array.

- `join(".", ["www", "google", "com"]) == "www.google.com"`
- `join([9, 9], [[1], [2, 3]]) == [ 1, 9, 9, 2, 3 ]`

## `lines`

_Available since version 0.10.0._

Concatenate an array of strings into a text file with newline characters after each string. This is suitable for constructing bash scripts and the like.

## `flattenArrays`

_Available since version 0.10.0._

Concatenate an array of arrays into a single array.

Example: `flattenArrays([[1, 2], [3, 4], [[5, 6], [7, 8]]]) == [ 1, 2, 3, 4, [ 5, 6 ], [ 7, 8 ] ]`

## `reverse`

_Available since version 0.13.0._

Reverses an array.

## `sort`

_Available since version 0.10.0._

`std.sort(arr, keyF=id)` sorts the array using the `<=` operator.

Optional argument `keyF` is a single argument function used to extract comparison key from each array element.

## `uniq`

_Available since version 0.10.0._

`uniq(arr, keyF=id)` removes successive duplicates. When given a sorted array, removes all duplicates.

Optional argument `keyF` is a single argument function used to extract comparison key from each array element.

## `all`

_Available since version 0.19.0._

`all(arr)` returns `true` if all elements of the input array are `true`, `false` otherwise. `all([])` evaluates to `true`.

It's an error if `arr` is not an array, or `arr` contains non-boolean values.

## `any`

_Available since version 0.19.0._

`any(arr)` return `true` if any element of `arr` is `true`, `false` otherwise. `any([])` evaluates to `false`.

It's an error if `arr` is not an array, or `arr` contains non-boolean values.

## `sum`

_Available since version 0.20.0._

Returns the sum of all the elements.

## `avg`

_Available since version 0.20.0._

Returns the average of all the elements.

## `set`

_Available since version 0.10.0._

`set(arr, keyF=id)` is a shortcut for `uniq(sort(arr))`.

## `setInter`

_Available since version 0.10.0._

`setInter(a, b, keyF=id)` is the set intersection operation (values in both `a` and `b`).

`a` and `b` must be sets, i.e. sorted arrays with no duplicates. If that is not the case, this function will quietly return non-meaningful results.

The optional `keyF` function can be used to extract a key to use from each element. This key is used for the purpose of identifying uniqueness.

## `setUnion`

_Available since version 0.10.0._

`setUnion(a, b, keyF=id)` is the set union operation (values in any of `a` or `b`).

Note that `+` on sets will simply concatenate the arrays, possibly forming an array that is not a set (due to not being ordered without duplicates).

Examples:

- `setUnion([1, 2], [2, 3]) == [ 1, 2, 3 ]`
- `setUnion([{n:"A", v:1}, {n:"B"}], [{n:"A", v: 9999}, {n:"C"}], keyF=function(x) x.n) == [ { "n": "A", "v": 1 }, { "n": "B" }, { "n": "C" } ]`

`a` and `b` must be sets, i.e. sorted arrays with no duplicates. If that is not the case, this function will quietly return non-meaningful results.

The optional `keyF` function can be used to extract a key to use from each element. This key is used for the purpose of identifying uniqueness.

## `setDiff`

_Available since version 0.10.0._

`setDiff(a, b, keyF=id)` is the set difference operation (values in `a` but not `b`).

`a` and `b` must be sets, i.e. sorted arrays with no duplicates. If that is not the case, this function will quietly return non-meaningful results.

The optional `keyF` function can be used to extract a key to use from each element. This key is used for the purpose of identifying uniqueness.

## `setMember`

_Available since version 0.10.0._

`setMember(x, s, keyF=id)` returns `true` if `x` is a member of `s`, otherwise `false`.

`s` must be a set, i.e. a sorted array with no duplicates. If that is not the case, this function will quietly return non-meaningful results.

The optional `keyF` function can be used to extract a key to use from each element. This key is used for the purpose of identifying uniqueness.

## `get`

_Available since version 0.18.0._

`get(o, f, default=null, inc_hidden=true)` returns the object `o`'s field `f` if it exists or `default` value otherwise. `inc_hidden` controls whether to include hidden fields.

## `objectHas`

_Available since version 0.10.0._

`objectHas(o, f)` returns `true` if the given object `o` has the field `f` (given as a string), otherwise `false`.

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

Like `objectHas` but also includes hidden fields.

## `objectFieldsAll`

_Available since version 0.10.0._

Like `objectFields` but also includes hidden fields.

## `objectValuesAll`

_Available since version 0.17.0._

Like `objectValues` but also includes hidden fields.

## `objectKeysValuesAll`

_Available since version 0.20.0._

Like `objectKeysValues` but also includes hidden fields.

## `mapWithKey`

_Available since version 0.10.0._

`mapWithKey(func, obj)` applies the given `func` to all fields of the given `obj`, also passing the field name.

The function `func` is expected to take the field name as the first parameter and the field value as the second.

## `base64`

_Available since version 0.10.0._

Encodes the given value into a base64 string.

The encoding sequence is `A-Za-z0-9+/` with `=` to pad the output to a multiple of 4 characters.

The value can be a string or an array of numbers, but the codepoints / numbers must be in the 0 to 255 range.

The resulting string has no line breaks.

## `base64DecodeBytes`

_Available since version 0.10.0._

`base64DecodeBytes(str)` decodes the given base64 string into an array of bytes (number values).

Currently assumes the input string has no linebreaks and is padded to a multiple of 4 (with the `=` character). In other words, it consumes the output of `base64`.

## `base64Decode`

_Available since version 0.10.0._

**Deprecated**, use `base64DecodeBytes` and decode the string explicitly (e.g. with `decodeUTF8`) instead.

Behaves like `base64DecodeBytes` except returns a naively encoded string instead of an array of bytes.

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

`mergePatch(target, patch)` applies `patch` to `target` according to [RFC7396](https://tools.ietf.org/html/rfc7396).

## `trace`

_Available since version 0.11.0._

`trace(str, rest)` outputs the given string `str` to stderr and returns `rest` as the result.

Example:

```jsonnet
local conditionalReturn(cond, in1, in2) =
  if cond then
    std.trace('cond is true, returning ' + std.toString(in1), in1)
  else
    std.trace('cond is false, returning ' + std.toString(in2), in2);

{
  a: conditionalReturn(true, { b: 1 }, { c: 2 }),
}
```

Prints:

```
TRACE: test.jsonnet:3 cond is true, returning {"b": 1}
{
  "a": {
    "b": 1
  }
}
```
