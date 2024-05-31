# Tokens

These are some tokens in Jsonnet.

## `:::`

Another kind of field marker.

## `!=`

Inequality comparator.

## `&&`

Logical and. Short-circuits.

## `::`

A kind of field marker.

Fields with this marker will not be included in the output JSON when materialized.

## `<<`

Bit-shift left.

## `<=`

Less than or equal to comparator.

## `==`

Equality comparator.

## `>=`

Greater than or equal to comparator.

## `>>`

Bit-shift right.

## `||`

Logical or. Short-circuits.

## `!`

Logical not.

## `$`

A variable that refers to the root object.

## `%`

1. Compute numerical modulus.

   ```jsonnet
   assert 10 % 3 == 1;
   ```

2. Format things into a string.

   ```jsonnet
   assert "hi %s, how are you doing %s?" % ["there", "today"]
     == "hi there, how are you doing today?";
   ```

## `&`

Bitwise and.

## `(`

1. Override the default expression parsing precedence.
2. Begin a parameter or argument list in a function definition or call.

## `)`

The companion of `(`.

## `*`

Multiplication.

## `+`

Addition, for numbers, strings, arrays, or objects.

```jsonnet
local num = 1 + 2;
local str = "hello, " + "world";
local ary = [1, 2] + [8, 9];
assert { a: -321, b: str } + { a: num, c: ary }
  == { a: 3, b: "hello, world", c: [1, 2, 8, 9] };
```

## `,`

Separate elements in a parameter list, argument list, array, object, etc.

## `-`

Numerical negation.

## `.`

Get an object's field. Can be chained.

```jsonnet
local a = { foo: { bar: "quz" } };
assert a.foo.bar == "quz";
```

## `/`

Numerical division.

## `:`

The default field marker.

## `;`

Separate a `local`'s bindings from the expression the whole `local` evaluates to.

## `<`

Less than comparator.

## `=`

Assign an expression to a binding.

## `>`

Greater than comparator.

## `[`

Begin an array or array subscript.

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

```jsonnet
local f = importbin "foo.o";
assert std.length(f) == 123;
```

## `importstr`

Import a file as a string.

```jsonnet
local f = importbin "hi.txt";
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

```jsonnet
// a.jsonnet
1 + 2

// b.jsonnet
local a = import "a.jsonnet";
a + 4 // 7
```

## `error`

Emit an error.

```jsonnet
local hm(x) = if x > 2 then x + 4 else error "oops";
local seven = hm(3);
hm(0) // error: oops
```

## `false`

The "no" boolean. Opposite of `true`.

## `local`

Create some bindings (usually just one).

```jsonnet
local three = 1 + 2;
assert three + 4 == 7;
```

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

assert isOdd(4) == false;
```

## `super`

The parent (left) object in a chain of objects connected together with `+`.

```jsonnet
assert { a: 3 } + { b: super.a + 1 }
  == { a: 3, b: 4 };
```

## `else`

Denotes what an `if` expression evaluates to if the condition is `false`.

## `null`

A value representing "nothing".

## `self`

The current object.

## `then`

Denotes what an `if` expression evaluates to if the condition is `true`.

## `true`

The "yes" boolean. Opposite of `false`.

## `for`

Loop over an array to create an array or object via a comprehension.

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

Denotes the thing to loop over in a `for`.
