# Changelog

Things under "main" are not released yet.

## v0.9.7

Add types for the following functions:

- `std.contains`
- `std.deepJoin`
- `std.equalsIgnoreCase`
- `std.flattenDeepArray`
- `std.maxArray`
- `std.minArray`
- `std.objectRemoveKey`
- `std.remove`
- `std.removeAt`
- `std.sha1`
- `std.sha256`
- `std.sha3`
- `std.sha512`
- `std.trim`

## v0.9.6

- Support `|||-` text block syntax which removes the final newline.
- Improve tuple types.
- Handle blank lines in `|||` text blocks.
- For Linux, build on Ubuntu 22 to get older glibc.

## v0.9.5

- Error on `==` where both sides are functions.
- Error on comparison (`<`, `>`, etc) where the types cannot be compared.
- Add tuple types to the type system.
- Improve go-to-def for some object field cases.
- Parse (but do not allow) `tailstrict`.
- Improve typing of:
  - Subscripting on strings
  - `%`
  - `std.flatMap`
  - `std.format`
  - `std.mod`
  - `std.objectFields`
  - `std.objectFieldsAll`

## v0.9.4

- Set the current dir of `bin/jsonnetfmt` to the workspace root.

## v0.9.3

- Set the root dir to the workspace root, not the current directory of the language server process.

## v0.9.2

- Improve correctness of editing open files.

## v0.9.1

- Allow hidden and plus fields in object comprehensions.
- Add new std fields:
  - `std.pi`
  - `std.log2`
  - `std.log10`
  - `std.atan2`
  - `std.hypot`
  - `std.deg2rad`
  - `std.rad2deg`
  - `std.native`
- Improve typing of:
  - `[]` (give it array type, not set type)
  - `==` for functions (not allowed)
  - `+` for objects (lhs known field types are overridden when rhs has unknown)
  - `+` for known non-addable types and `any` (not allowed)
- Allow formatting jsonnet files via a local `bin/jsonnetfmt` executable in the workspace root; the full interface of such a formatter is described in the [manual](/docs/manual.md#rjsonnetformatengine).

## v0.9.0

- Improve syntax highlighting for the `function` type.
- Improve object field completions for unions.
- Do not show hover info on comments and whitespace.
- Add more specific std fn param names.
- Handle non-identifier field completions by wrapping them in `["..."]`.

## v0.8.6

- Improve negative flow typing, especially with object fields.

## v0.8.5

- Default extra dirs to `.` and `bazel-bin`.

## v0.8.4

- Improve go-to-def for object locals.
- Improve special case typing for array unions.

## v0.8.3

- Fix unused warnings for object comprehension variables.
- Make a small CLI.

## v0.8.2

- Improve signature help argument matching.
- Improve checking compatibility for union types.
- Improve display of types in diagnostics.

## v0.8.1

- Improve flow typing for variables with function type.
- Improve typing for:
  - `std.reverse`
  - `std.filter`
  - `std.filterMap`
- Allow hovering over function parameters to see their type.
- Allow writing `...` as a placeholder.

## v0.8.0

- Bring back ability to analyze flow types with `==` to a literal that is not the only value of that type.
- Improve object flow typing with `std.length`.
- Support `std.objectHasEx` in flow typing.
- Remove what little support we had for named arguments in flow typing.
- Support flow typing for array element types with `std.all(std.map(..., xs))`.

## v0.7.8

- Remove ability to analyze flow types with `==` to a literal that is not the only value of that type, e.g. `x == 3`. This still allows for flow typing information for cases like `x == null` and `x != null`.
- Improve correctness of flow typing in other cases.
- Allow `std.length` to affect flow typing, e.g. by marking object as not having unknown fields.

## v0.7.7

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
