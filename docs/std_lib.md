# Standard library

These are docs for the Jsonnet standard library, available on the global `std` object.

- Retrieved from https://jsonnet.org/ref/stdlib.html
- Applied some formatting improvements
- Reworded a bit
- Removed some unwieldy examples
- Filled in some doc of my own for less-documented functions
- Removed doc for not yet implemented functions

## `thisFile`

_Available since version 0.10.0._

Note that this is a field. It contains the current Jsonnet filename as a string.

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

<!-- @eval-error: not yet implemented: setUnion -->

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

`std.setMember(x, s, keyF=id)` whether `x` is a member of `s`.

`s` must be a set, i.e. a sorted array with no duplicates. If that is not the case, this function will quietly return non-meaningful results.

The optional `keyF` function can be used to extract a key to use from each element. This key is used for the purpose of identifying uniqueness.

## `get`

_Available since version 0.18.0._

`std.get(o, f, default=null, inc_hidden=true)` returns the object `o`'s field `f` if it exists or `default` value otherwise. `inc_hidden` controls whether to include hidden fields.

## `objectHas`

_Available since version 0.10.0._

`std.objectHas(o, f)` returns whether the given object `o` has the field `f` given as a string.

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

<!-- @eval-error: not yet implemented -->

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

## `equals`

Returns whether the two arguments equal each other.

## `objectHasEx`

`std.objectHasEx(obj, fname, hidden)` is the same as `std.objectHasAll(obj, fname)` when `hidden` is `true`, and the same as `std.objectHas(obj, fname)` when `hidden` is `false`.
