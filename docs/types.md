# Types

rjsonnet supports type inference and flow typing, including on local variables and across imported files, without introducing new syntax to Jsonnet.

## Annotations

The trickiest part of type inference without type annotations is handling function parameters.

Contrary to other languages like [OCaml][ocaml], [Haskell][haskell], and Standard ML, rjsonnet does not perform advanced [Damas-Hindley-Milner][dhm]-style type inference on function parameter types. We instead assume function parameters have `any` type.

However, you can "annotate" a function's parameter's types with `assert`s like this:

```jsonnet
local func(x, y) =
  assert std.isString(x) || std.isNumber(x);
  assert std.isBoolean(y);
  if x == 3 && y then
    "hi"
  else if std.isString(x) then
    x
  else
    std.toString(y)
;

func("hi", false)
## ^ type: (x: string | number, y: boolean) => string
```

## Flow typing

rjsonnet supports flow typing, where the type of variables is narrowed in different branches of `if` expressions based on certain tests on the variable.

Consider this example, where the type of `x` is narrowed in different `then` and `else` branches based on the `if` conditions:

```jsonnet
function(x)
  if std.isObject(x) then
    if "foo" in x then
      if std.isNumber(x.foo) then
        if std.length(x) == 1 then
          x.foo + 4
##        ^ type: { foo: number }
        else
          x.foo
##        ^ type: { foo: number, ... }
      else
        assert !std.isBoolean(x.foo) && x.foo != null;
        std.length(x.foo)
##                 ^ hover: { foo: string | array[any] | ((...) => any) | object, ... }
    else
      std.length(x)
##               ^ type: object
  else
    std.length(std.toString(x))
##                          ^ type: boolean | null | string | number | array[any] | ((...) => any)
```

For flow typing, rjsonnet understands how the runtime behavior of certain expressions depends on the types of values, and thus uses that information to infer static types. We call these certain expressions the "flow tests".

These flow tests are the same ones supported in `assert`s, which, when at the very beginning of a function, serve as type annotations for the function parameters.

They affect the static types of expressions which are either variables, or a chain of field subscripts with known field names ending in a variable. Like these:

- `x`
- `x.a`
- `x["b"]["c"].d.e`

Many of the flow tests involve certain `std` functions. Note that we only support passing positional arguments for these functions for the purposes of the flow tests. That is, if you pass named parameters, it'll still work at runtime, but no flow typing information will be inferred statically.

The supported flow tests are as follows.

### Unary functions returning `boolean`

- `std.isArray`
- `std.isBoolean`
- `std.isNumber`
- `std.isObject`
- `std.isString`
- `std.isFunction`
- `std.isEven`
- `std.isOdd`
- `std.isInteger`
- `std.isDecimal`

### `std.type`

Similar to `std.isArray`, etc, we also support `std.type(expr) == STR`, where `STR` is one of the possible return values of that function, which are:

- `"number"`
- `"string"`
- `"boolean"`
- `"array"`
- `"object"`
- `"function"`
- `"null"`

### Comparison to literals

`expr == LIT`, where `LIT` is a literal, will infer that `expr` is the type of `LIT`.

### Field membership

These check for field membership on objects:

- `STR in expr`, where `STR` is a literal string
- `std.objectHas(expr, STR)`, where `STR` is a literal string
- `std.objectHasAll(expr, STR)`, where `STR` is a literal string
- `std.objectHasEx(expr, STR, hidden)`, where `STR` is a literal string and `hidden` is an expression

### Length

`std.length(x) == NUM`, where `NUM` is a literal number, can be used on objects and functions.

For objects, if `NUM` is the number of known fields, the object will then be marked as having no unknown fields.

For functions, if the function's argument count was not known before, it will be known after.

```jsonnet
function(f)
  assert std.isFunction(f);
  if std.length(f) == 2 then
    f
##  ^ type: ($a: any, $b: any) => any
  else
    f
##  ^ type: (...) => any
```

### Array element types

`std.all(std.map(elem_test, expr))` checks that `expr` is an array whose element type is given by `elem_test`.

`elem_test` can be either:

- One of the unary functions returning `boolean` listed as a flow test, but NOT called on anything
- A lambda function `function(x) a`, where `x` is a variable name and `a` is a flow test on `x`

```jsonnet
function(xs)
  assert std.isArray(xs);
  if std.all(std.map(std.isNumber, xs)) then
    std.sum(xs)
##          ^^ type: array[number]
  else if std.all(std.map(function(x) std.isString(x) || std.isObject(x), xs)) then
    std.length(xs) + 1
##             ^^ type: array[string | object]
  else
    std.length(xs) + 2
##             ^^ type: array[any]
```

### Conjunction

`a && b`, where `a` and `b` are flow tests, chains together the tests `a` and `b`. This is similar to if we had done the tests one after another:

- `if a && b then ...` is like `if a then if b then ...`
- `assert a && b; ...` is like `assert a; assert b; ...`

### Disjunction

`a || b`, where `a` and `b` are flow tests, tests if either `a` or `b` is true. This often leads to union types:

```jsonnet
function(x)
  if std.isString(x) || std.isNumber(x) then
    x
##  ^ type: string | number
```

### Negation

`!a`, where `a` is a flow test, negates the meaning of the test.

This is useful for e.g. `expr != null`, which can narrow away the nullability of a nullable type (aka a union type with `null`).

This works because `a != b` desugars to `!(a == b)`, and `expr == LIT` is a supported test.

The negation of a flow test is also reflected in the `else`-branch of `if` expressions:

```jsonnet
function(x)
  assert std.isNumber(x) || x == null;
  if x == null then
##   ^ type: null | number
    3
  else
    x
##  ^ type: number
```

## Type syntax

Although we don't introduce new syntax for types to the Jsonnet language, we do use a certain syntax to report types to the user.

### Unknown type

`any` is the "type" of things that we don't know the type of.

This is like [`any`][ts-any] in [TypeScript][ts] or [`T.untyped`][t-untyped] in [Sorbet][sorbet], a Ruby type checker.

### Primitive types

They are:

- `number`
- `string`
- `true`
- `false`
- `null`

These are fairly self-explanatory. Notably, the values `true`, `false`, and `null` have their own types.

`boolean` is the type that is the union of `true` and `false`. See docs for [union types](#union-types).

### Array types

`array[T]` is the type of arrays where the elements have type `T`.

### Set types

`set[T]` is the type of sets where the elements have type `T`.

At runtime, sets are represented as sorted, duplicate-free arrays. Thus, for a type `T`, the type `set[T]` "decays" to the type `array[T]`. For instance, if you have `x` with type `set[T]`, and your function `f` accepts a `array[T]`, you may pass `x` to `f`.

This is called "subtyping" in programming language theory jargon.

### Object types

`{ foo: T1, bar: T2 }` is the type of an object with two known fields: `foo` of type `T1`, and `bar` of type `T2`, and no unknown fields.

If an object may have unknown fields, an extra `...` is added at the end.

`object` is the type of an object with no known fields that may have unknown fields. It is equivalent to `{ ... }`, but we prefer to show it as the former. It is distinct from `{}`, the type of the object known to have no fields.

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

Order does not matter in union types. So `T1 | T2` is the same as `T2 | T1`.

`never` is the union of zero types, aka the empty union type. It is a type with no values. It is the type of expressions that are known to be impossible to evaluate, like `error` expressions and expression that have "nonsensical" types.

```jsonnet
function(x)
  if std.isNumber(x) && !std.isNumber(x) then
    x
##  ^ type: never
  else if std.isNumber(x) then
    x + 1
##  ^ type: number
  else
    error "whoops"
##  ^^^^^^^^^^^^^^ type: never
```

### Function types

`(x: T1) => T2` is the type of functions with one required parameter `x` of type `T1` that return type `T2`.

If a function has multiple parameters, they are separated with `,`. If it has no parameters, the parameter list is shown as `()`.

When a parameter is optional, it has a `?` after the argument name, like `(x?: T1) => T2`.

When a function's parameters are unknown, they are shown as `...`.

### Precedence

There is some ambiguity in the type syntax that can be resolved by specifying the binding precedence of the different bits of syntax.

Union types bind stronger than function types. This means 1 and 2 below are equivalent, and distinct from 3.

1. `(x: boolean) => number | string`
2. `(x: boolean) => (number | string)`
3. `((x: boolean) => number) | string`

[dhm]: https://bernsteinbear.com/blog/type-inference/
[sorbet]: https://sorbet.org/
[ts]: https://www.typescriptlang.org/
[ts-any]: https://www.typescriptlang.org/docs/handbook/2/everyday-types.html#any
[t-untyped]: https://sorbet.org/docs/untyped
[ocaml]: https://ocaml.org/
[haskell]: https://www.haskell.org/
