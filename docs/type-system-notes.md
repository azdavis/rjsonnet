# Thinking through the type system

Some notes and examples on the type system to help explain it and/or make sure it makes sense.

## Sub-typing

- want: `number`
- got: `3`

3 is a number.

OK

- want `3`
- got `number`

i got an arbitrary number, but i wanted 3 specifically.

NOT OK

## Contra-variance in function arguments

- want `(a: 3) => string`
- got `(a: number) => string`

i wanted a fn i can call on 3.

i got a fn i can call on 3 or any other number.

OK

- want: `(a: number) => string`
- got: `(a: 3) => string`

i wanted a fn i can call on any number, like 3 or 4 or 123.

i got a fn i can only call on 3.

what if i wanted to call it on 4?

NOT OK

## Handling default arguments

- want `(a: number, b: number) => number`
- got `(a: number, b?: number) => number`

i wanted a fn i can call on 2 numbers.

i got a fn i can call on 2 numbers or 1 number.

OK

- want: `(a: number, b?: number) => number`
- got: `(a: number, b: number) => number`

i wanted a fn i can call on 1 or 2 numbers.

i got a fn i can only call on 2 numbers.

## Importance of argument naming

- want: `(a: number) => number`
- got: `(b: number) => number`

i wanted a fn i can call like this:

- `f(3)`
- `f(a=3)`
- `f(4)`

i got a fn i can call like this:

- `f(3)`
- `f(b=3)`
- `f(4)`

but NOT like this: `f(a=3)`

NOT OK

## Importance of argument order

- want: `(a: number, b: string) => number`
- got: `(b: string, a: number) => number`

i wanted a fn i can call like this:

- `f(3, "hi")`
- `f(a=3, b="hi")`
- `f(b="hi", a=3)`

i got a fn i can call like this:

- `f("hi", 3)`
- `f(a=3, b="hi")`
- `f(b="hi", a=3)`

but NOT like this: `f(3, "hi")`

NOT OK

## Want union

- want: `number | string`
- got: `number`

OK

## Got union

- want: `number`
- got: `number | string`

what if it's a string?

NOT OK

## Want and got union

- want: `number | boolean | string`
- got: `number | string`

OK

## Want and got union, where one of the union elements in got is more specific

- want: `number | boolean | string`
- got: `3 | string`

OK

## Takeaways

- contravariant in parameter types
- argument order is part of the type
- argument name is part of the type
- if want has a required param, must be in got as required or optional
- if want has an optional param, must be in got as optional, NOT required
- if got have extra optional params at the end, it's ok (not shown in examples)
- if got have extra required params anywhere, it's NOT ok (not shown in examples)
- if want a union and got a non-union, got must unify with ANY of the parts of want
- if got a union, split got into non-union parts and ensure ALL parts unify with want (whether want be a union or not)
