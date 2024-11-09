# Types

rjsonnet supports type inference, including across imported files, and on local variables, without introducing new syntax to Jsonnet.

## Syntax

Although we don't introduce new syntax to the source language, we do use a certain syntax to report types to the user.

The syntax is roughly that of TypeScript's type syntax.

### Unknown type

`any` is the "type" of things that we don't know the type of.

This is like `any` in TypeScript or `T.untyped` in Sorbet, a Ruby type checker.

### Primitive types

They are:

- `number`
- `string`
- `true`
- `false`
- `null`

These are fairly self-explanatory. Notably, the values `true`, `false`, and `null` have their own types.

The type that is the union of `true` and `false` is shown as `boolean`.

### Array types

`T[]` is the type of arrays where the elements have type `T`.

### Object types

`{ foo: T1, bar: T2 }` is the type of an object with two known fields: `foo` of type `T1`, and `bar` of type `T2`, and no unknown fields.

If an object may have unknown fields, an extra `...` is added at the end.

`object` is the type of an object with no known fields that may have unknown fields. It is equivalent to `{ ... }`, but we prefer to show it as the former.

### Union types

`T1 | T2` is the type of values that may be either of type `T1` or of type `T2`.

Unions of more than two types are allowed, like `T1 | T2 | T3`.

When union types are union'd with other union types, the unions are flattened. That is, these are all the same:

- `T1 | (T2 | T3)`
- `(T1 | T2) | T3`
- `T1 | T2 | T3`

But we prefer the final, flattened form.

Duplicate types are also eliminated. So we prefer to show `T1 | T2 | T1` as `T1 | T2`.

If a union type contains both `true` and `false`, instead of showing the type as `true | false | ...`, it is shown as `boolean | ...`.

Order does not matter in union types.

`never` is the union of 0 types, aka the empty union type. It is a type with no values. This is the type of `error` expressions.

### Function types

`(x: T1) => T2` is the type of functions with one required parameter `x` of type `T1` that return type `T2`.

When a parameter is optional, it has a `?` after the argument name, like `(x?: T1) => T2`.

### Precedence

There is some ambiguity in the type syntax that can be resolved by specifying the binding precedence of the different bits of syntax.

Union types bind stronger than function types. This means 1 and 2 below are equivalent, and distinct from 3.

1. `(x: boolean) => number | string`
2. `(x: boolean) => (number | string)`
3. `((x: boolean) => number) | string`

Array types bind stronger than union types. This means 1 and 2 below are equivalent, and distinct from 3.

1. `number | string[]`
2. `number | (string[])`
3. `(number | string)[]`

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
