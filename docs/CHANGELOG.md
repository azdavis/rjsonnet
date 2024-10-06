# Changelog

## main

- Fix types for `std.slice` params.
- Improve hover for `local` binds.
- Improve go-to-def, error, etc. locations for `local f(x) = ...` function parameters and `for x in xs` object and array comprehensions.
- Improve error locations for duplicate binding errors.
- Allow for type "annotations" via `assert std.isTYPE(x)` at the beginning of a function.

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
