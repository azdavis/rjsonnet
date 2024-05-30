# Tokens

These are some tokens in Jsonnet.

## `:::`

Another kind of field marker.

## `!=`

Inequality comparator.

## `&&`

Logical and. Short-circuits.

## `::`

A kind of field marker. Fields with this marker will not be included in the output JSON when materialized.

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
   10 % 3 // 1
   ```

2. Format things into a string.

   ```jsonnet
   "hi %s, how are you doing %s?" % ["there", "today"]
   // "hi there, how are you doing today?"
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
local two = 1 + 2;
local greeting = "hello, " + "world";
local one_two_eight_nine = [1, 2] + [8, 9];
{ a: 1, b: 2 } + { a: 3, c: 4 }
// { a: 3, b: 2, c: 4 }
```

## `,`

Separate elements in a parameter list, argument list, array, object, etc.

## `-`

Numerical negation.

## `.`

Get an object's field.

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
std.length(f)
```

## `importstr`

Import a file as a string.

```jsonnet
local f = importbin "hi.txt";
std.length(f)
```

## `function`

Begin a function expression.

```jsonnet
function(a, b)
  local c = a + b;
  c * c
```

## `assert`

Begin an assert expression.

```jsonnet
assert 2 + 2 == 4;
assert 1 < 2 : "one is smaller than two";

{
  foo: 3,
}
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

Create a binding.

## `super`

The parent (left) object in a chain of objects connected together with `+`.

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

Loop over an array to create an array or object comprehension.

```jsonnet
local addOne(xs) = [x + 1 for x in xs];
local ys = addOne([2, 5]); // [3, 6]

local lengths(xs) = { [x]: std.length(x) for x in xs };
lengths(["foo", "hi"]) // { foo: 3, hi: 2 }
```

## `if`

Branch on a boolean.

## `in`

Denotes the thing to loop over in a `for`.
