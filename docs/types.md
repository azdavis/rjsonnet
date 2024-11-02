# Types

rjsonnet supports type inference, including across imported files, and on local variables, without introducing new syntax to Jsonnet.

## Syntax

Although we don't introduce new syntax to the source language, we do use a certain syntax to report types to the user.

The syntax is roughly that of TypeScript's type syntax:

- "Unknown" type: `any`
  - This is like `any` in TypeScript or `T.untyped` in Sorbet, a Ruby type checker.
- Primitive types: `number`, `string`, `true`, `false`, `null`
  - These are fairly self-explanatory.
  - Notably, the values `true`, `false`, and `null` have their own types.
  - The common type that is the union of `true` and `false` is written as `boolean`.
- Array types: `T[]`
  - This is an array where the elements have type `T`.
- Object types: `{ foo: T1, bar: T2 }`
  - This is an object with two known fields: `foo` of type `T1`, and `bar` of type `T2`, and no unknown fields.
  - If an object may have unknown fields, an extra `...` is added at the end.
  - The common type `{ ... }` (an object with no known fields that may have unknown fields) is written as `object`.
- Union types: `T1 | T2`
  - A value of this type may be either of type `T1` or of type `T2`.
  - The empty union type (the type with no values, which is the type of `error` expressions) is written as `never`.
- Function types: `(x: T1) => T2`
  - Optional arguments have a `?` after the argument name.

## Annotations

The trickiest part of type inference without type annotations is handling function parameters.

Contrary to other languages like OCaml, Haskell, and Standard ML, rjsonnet does not perform advanced Damas-Hindley-Milner-style type inference on function parameter types. We instead assume function parameters have `any` type.

However, you can "annotate" a function's parameter's types with `assert`s like this:

```jsonnet
local f(a, b) =
  assert std.isNumber(a) || std.isString(a);
  assert std.isBoolean(b);
  if a == 3 && b then
    "hi"
  else if std.isString(a) then
    a
  else
    std.toString(b)
;
```

This will be inferred to have type:

```
(a: string | number, b: boolean) => string
```
