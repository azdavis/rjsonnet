# Tokens

These are some tokens in Jsonnet.

## `:::`

Another kind of field marker.

```jsonnet
{
  regular: 4,
  hidden:: 3,
  somethingElse:: 7,
}
```

## `!=`

Inequality comparator.

```jsonnet
assert 2 + 2 != 5;
```

## `&&`

Logical and. Short-circuits.

This does not error:

```jsonnet
assert 1 < 2 && 4 > 3;
assert !(1 == 2 && (error "nope"));
```

## `::`

A kind of field marker.

Fields with this marker will not be included in the output JSON when materialized.

```jsonnet
{
  regular: 4,
  hidden:: 3,
}
```

## `<<`

Bit-shift left.

```jsonnet
assert 1 << 2 == 4;
```

## `<=`

Less than or equal to comparator.

```jsonnet
assert 1 <= 2;
assert 2 <= 2;
```

## `==`

Equality comparator.

```jsonnet
assert 2 + 2 == 4;
```

## `>=`

Greater than or equal to comparator.

```jsonnet
assert 3 >= 2;
assert 2 >= 2;
```

## `>>`

Bit-shift right.

```jsonnet
assert 4 >> 2 == 1;
```

## `||`

Logical or. Short-circuits.

This does not error:

```jsonnet
assert 1 == 1 || (error "not evaluated");
```

## `!`

Logical not.

```jsonnet
assert !false;
```

## `$`

A variable that refers to the root object.

```jsonnet
{
  a: 3,
  b: 4,
  c: {
    d: {
      e: $.a,
    },
  },
}
```

## `%`

1. Compute numerical modulus.

    <!-- @eval-error: not yet implemented: std.mod -->

   ```jsonnet
   assert 10 % 3 == 1;
   ```

2. Format things into a string.

    <!-- @eval-error: not yet implemented: std.mod -->

   ```jsonnet
   assert "hi %s, how are you doing %s?" % ["there", "today"]
     == "hi there, how are you doing today?";
   ```

## `&`

Bitwise and.

```jsonnet
assert (4 & 5) == 4;
```

## `(`

1. Override the default expression parsing precedence.

   ```jsonnet
   assert 1 + 2 * 3 == 7;
   assert (1 + 2) * 3 == 9;
   ```

2. Begin a parameter or argument list in a function definition or call.

   ```jsonnet
   local foo(x, y) = if x then y;
   assert foo(true, 4) == 4;
   ```

## `)`

The companion of `(`.

## `*`

Multiplication.

```jsonnet
assert 3 * 5 == 15;
```

## `+`

Addition, for numbers, strings, arrays, or objects.

<!-- @eval-error: not yet implemented: object-object equality -->

```jsonnet
local num = 1 + 2;
local str = "hello, " + "world";
local ary = [1, 2] + [8, 9];
assert { a: -321, b: str } + { a: num, c: ary }
  == { a: 3, b: "hello, world", c: [1, 2, 8, 9] };
```

## `,`

Separate elements in a parameter list, argument list, array, object, etc.

```jsonnet
local xs = [1, 2, 3];
local ys = { a: 4, b: 6 };
local f(a, b) = [a, b];
f(ys, xs)
```

## `-`

Numerical negation and subtraction.

```jsonnet
local negThree = -3;
assert negThree - 2 == -5;
```

## `.`

Get an object's field. Can be chained.

```jsonnet
local a = { foo: { bar: "quz" } };
assert a.foo.bar == "quz";
```

## `/`

Numerical division.

```jsonnet
assert 15 / 5 == 3;
```

## `:`

The default field marker.

```jsonnet
{ foo: 3, bar: false }
```

## `;`

Separate a `local` or `assert` from the rest of the expression.

```jsonnet
local x = 3;
assert x + 1 == 4;
```

## `<`

Less than comparator.

```jsonnet
assert 4 < 5;
```

## `=`

Assign an expression to a binding.

```jsonnet
local x = 4;
assert x + 1 == 5;
```

## `>`

Greater than comparator.

```jsonnet
assert 7 > 3;
```

## `[`

Begin an array or array subscript.

```jsonnet
local xs = [1, 5, 7];
assert xs[1] - 1 == 4;
```

## `]`

The companion of `[`.

## `^`

Bitwise xor.

## `{`

Begin an object.

## `|`

Bitwise or.

## `}`

The companion of `{`.

## `~`

Bitwise not.

## `importbin`

Import a file as raw bytes.

<!-- @pre-eval-error: path not found: foo.zip -->

```jsonnet
local f = importbin "foo.zip";
assert std.length(f) == 123;
```

## `importstr`

Import a file as a string.

<!-- @pre-eval-error: path not found: hi.txt -->

```jsonnet
local f = importstr "hi.txt";
assert std.length(f) == 456;
```

## `function`

Begin a function expression.

```jsonnet
local hm = function(a, b)
  local c = a + b;
  c * c;

assert hm(1, 2) == 9;
```

You can also use syntax sugar with `local` when binding the function to a variable, instead of using the `function` keyword.

```jsonnet
local hm(a, b) =
  local c = a + b;
  c * c;

assert hm(1, 2) == 9;
```

## `assert`

Begin an assert expression.

```jsonnet
assert 2 + 2 == 4;
assert 1 < 2 : "one is smaller than two";
```

Can appear inside objects too.

```jsonnet
{
  assert 2 + 2 == 4,
  foo: 3,
}
```

## `import`

Import a Jsonnet file.

If `a.jsonnet` contains:

```jsonnet
1 + 2
```

And in the same folder `b.jsonnet` contains:

<!-- @pre-eval-error: path not found: a.jsonnet -->

```jsonnet
local a = import "a.jsonnet";
a + 4
```

Then `b.jsonnet` will evaluate to `7`.

## `error`

Emit an error.

<!-- @eval-error: oops -->

```jsonnet
local hm(x) = if x > 2 then x + 4 else error "oops";
assert hm(3) == 7;
hm(0)
```

## `false`

The "no" boolean. Opposite of `true`.

## `local`

Create some bindings (usually just one).

```jsonnet
local three = 1 + 2;
assert three + 4 == 7;
```

You can also use syntax sugar with `local` when binding a function to a variable, instead of using the `function` keyword.

```jsonnet
local hm(a, b) =
  local c = a + b;
  c * c;

assert hm(1, 2) == 9;
```

<!-- TODO fix this bug (something with mutual recursion in the envs) -->
<!-- @eval-error: not in scope: `isEven` -->

The bindings, separated by `,`, may be mutually recursive.

```jsonnet
local
  isOdd(x) =
    if x == 0 then false
    else x == 1 || isEven(x - 1)
, isEven(x) =
    assert x >= 0 : "cannot figure out negative numbers";
    x == 0 || isOdd(x - 1)
;

assert !isOdd(4);
assert isOdd(3);
assert isEven(6);
```

## `super`

The parent (left) object in a chain of objects connected together with `+`.

<!-- @eval-error: not yet implemented: object-object equality -->

```jsonnet
assert { a: 3 } + { b: super.a + 1 }
  == { a: 3, b: 4 };
```

## `else`

Denotes what an `if` expression evaluates to if the condition is `false`.

```jsonnet
assert (if 1 > 2 then 3 else 4) == 4;
```

## `null`

A value representing "nothing".

## `self`

The current object.

## `then`

Denotes what an `if` expression evaluates to if the condition is `true`.

```jsonnet
assert (if 1 < 2 then 3 else 4) == 3;
```

## `true`

The "yes" boolean. Opposite of `false`.

## `for`

Loop over an array to create an array or object via a comprehension.

<!-- @eval-error: not yet implemented: std.makeArray -->

```jsonnet
local addOne(xs) = [x + 1 for x in xs];
assert addOne([2, 5]) == [3, 6];

local lengths(xs) = { [x]: std.length(x) for x in xs };
assert lengths(["foo", "hi"])
  == { foo: 3, hi: 2 };
```

## `if`

Branch on a boolean.

```jsonnet
local hm(x) = if x then 4 else 5;
assert hm(true) == 4;
assert hm(false) == 5;
```

You can skip the `else`. It defaults to `else null`.

```jsonnet
local hm(x) = if x then 4;
assert hm(true) == 4;
assert hm(false) == null;
```

## `in`

1. Denotes the thing to loop over in a `for`.

    <!-- @eval-error: not yet implemented: std.makeArray -->

   ```jsonnet
   local xs = [1, 3];
   assert [x + 1 for x in xs]
     == [2, 4];
   ```

1. Tests for field membership in objects.

    <!-- @eval-error: not yet implemented: std.objectHasAll -->

   ```jsonnet
   assert "foo" in { foo: 3 };
   assert !("bar" in { foo: 3 });
   ```
