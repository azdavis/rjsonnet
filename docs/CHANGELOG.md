# Changelog

Things under "main" are not released yet.

## main

- Tweak some diagnostic messages.
- Warn on some unreachable code cases.

## v0.7.6

- Improve display of object types with non-identifier field names.
- Improve typing for `std.prune`.
- Warn when adding two sets with `+`.
- Improve logic for when to show completions.
- Show documentation for std fields in completions.

## v0.7.5

- Improve type syntax highlighting.
- Use `array[T]` instead of `T[]` for array types.
- Add did-you-mean suggestions for unknown fields and undefined variables.

## v0.7.4

- Improve token docs.
- Add more examples to docs.
- Add handling for set types, which "decay" to array types.

## v0.7.3

- Allow `std.isFunction` and similar to learn that variables have function type.
- Add more examples to std lib docs.
- Improve handling of invalid text blocks.
- Error on invalid `super` usage.
- Error on invalid `import` with a text block.
- Support flow typing for checking object field membership.
- Support flow typing for checking object field types.

## v0.7.2

- Improve go-to-def, etc through asserts.
- Improve typing for `self`, `super`, and `$`.
- Show type of fully unknown object as `object` instead of `{ ... }`.
- Support flow typing for negated conditionals. This helps when e.g. narrowing a type that might be `null` to be not `null`.
- Improve type checking of `+` with union types.
- Improve formatting of union types with `boolean`.
- Improve typing for:
  - `std.join`
  - `std.repeat`

## v0.7.1

- Improve typing for:
  - `std.splitLimit`
  - `std.splitLimitR`
  - `std.format`
  - `std.flatMap`
- Fix flow typing where e.g. in the `else` case after checking `if x == "hi"`, we thought that `x` could not be an arbitrary string.
- Check types of function default arguments against their asserted types.
- Improve typechecking for subscripting on union types.

## v0.7.0

- Improve typing for:
  - `std.join`
  - `std.manifestJsonEx`
  - `std.repeat`
  - `std.slice`
- Improve syntax highlighting for verbatim strings.
- Allow type annotation asserts to be chained with `||` to make union types.
- Support flow typing, where performing checks on a variable in an `if` condition affects the type of the variable in the `then` and `else` expressions.
- Infer types for variables with comparing with `==` in some situations.
- Allow type annotation asserts in objects.

## v0.6.1

- Improve signature help with named arguments.
- Improve go-to-def, etc across imported files.
- Improve go-to-def for object field functions.

## v0.6.0

- Implement signature help.
- Fix highlighting of `$`.
- Improve hover for type and go to def for local functions like `local func(x) = x + 1`.
- Weaken the typing for `std.foldl` and `std.foldr` to be more accurate.

## v0.5.3

- Improve typing for:
  - `%` (aka `std.mod`)
  - `std.count`
  - `std.filterMap`
  - `std.find`
  - `std.foldl`
  - `std.foldr`
  - `std.mapWithKey`
  - `std.member`
  - `std.repeat`
  - `std.slice`
  - `std.trace`

## v0.5.2

- Avoid an issue with infinite recursion when jumping to def of recursive bindings like e.g. `local x = x`.
- Fix the type of:
  - `std.stringChars`
  - `std.range`
- Improve typing for `==` (require RHS type to be compatible with LHS).
- Add special-case typing for:
  - `%` (aka `std.mod`)
  - `std.assertEqual`
  - `std.equals`
  - `std.filter`
  - `std.join`
  - `std.makeArray`
  - `std.objectKeysValues`
  - `std.objectKeysValuesAll`
  - `std.reverse`
  - `std.set`
  - `std.slice`
  - `std.sort`
  - `std.uniq`

## v0.5.1

- Fix types for `std.slice` params.
- Improve hover for `local` binds.
- Improve go-to-def, error, etc. locations for `local f(x) = ...` function parameters and `for x in xs` object and array comprehensions.
- Improve error locations for duplicate binding errors.
- Allow for type "annotations" via `assert std.isTYPE(x)` or `std.type(x) == "TYPE"` at the beginning of a function.

## v0.5.0

- Add type checking for std functions.
- Display types on hover for std functions.
- Provide completions for object fields.

## v0.4.1

- Make some micro-optimizations.
- Fix an issue where files were analyzed in the reverse order.
- Improve type checking of function calls.
- Add more std items and docs.
- Show multi-line object types with trailing `,`.
- Show functions with many parameters across many lines.
- Fix an issue where files were not correctly analyzed after being updated.
- Fix issues where types were incorrectly shared or combined across files.

## v0.4.0

- Allow for type-checking across files.
- Add some types for `std`.
- Format large object types across many lines.

## v0.3.1

- Add some local type-checking.
- Add some hover for type information.

## v0.3.0

- Greatly decrease memory usage by being more lazy.
- Add back hover for showing std lib and keyword docs.
- Add back manifesting on hover (but only behind a setting, and very WIP).
- Diagnose unused vars.

## v0.2.0

- Decrease initial startup time.
- Un-implement hover. (We hope to add it back later.)
- Remove some settings.

## v0.1.1

- Show the source in diagnostics.

## v0.1.0

Initial public release.
