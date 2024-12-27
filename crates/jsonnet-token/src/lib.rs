//! Info about tokens, the primitive unit of syntax.

#![allow(clippy::needless_raw_string_hashes)]

#[cfg(test)]
mod tests;

use indoc::indoc;

/// A token.
#[derive(Debug)]
pub struct Token {
  /// The text of the token.
  pub text: &'static str,
  /// The purposes of this token.
  ///
  /// INVARIANT: non-empty.
  pub purposes: &'static [TokenPurpose],
}

/// A way a token can be used.
#[derive(Debug)]
pub struct TokenPurpose {
  /// Documentation for this usage of the token.
  pub doc: &'static str,
  /// An example of using the token in this way.
  pub example: &'static str,
  /// The outcome of running the example.
  pub outcome: Result<(), Error>,
}

/// An error outcome.
#[derive(Debug)]
pub enum Error {
  /// Before evaluating.
  PreEval(&'static str),
  /// When evaluating.
  Eval(&'static str),
  /// Would stack overflow; skip.
  StackOverflow,
}

/// All the tokens.
pub const ALL: [Token; 51] = [
  Token {
    text: ":::",
    purposes: &[TokenPurpose {
      doc: indoc! {"
        The non-hidden field marker. Fields marked with this will be included in the output JSON.
      "},
      example: indoc! {r#"
        {
          regular: 4,
          hidden:: 3,
          shown::: 7,
        }
      "#},
      outcome: Ok(()),
    }],
  },
  Token {
    text: "!=",
    purposes: &[TokenPurpose {
      doc: indoc! {"
        Inequality comparator.
      "},
      example: indoc! {r#"
        assert 2 + 2 != 5;
      "#},
      outcome: Ok(()),
    }],
  },
  Token {
    text: "&&",
    purposes: &[TokenPurpose {
      doc: indoc! {"
        Logical and. Short-circuits.

        This does not error:
      "},
      example: indoc! {r#"
        assert 1 < 2 && 4 > 3;
        assert !(1 == 2 && (error "nope"));
      "#},
      outcome: Ok(()),
    }],
  },
  Token {
    text: "::",
    purposes: &[TokenPurpose {
      doc: indoc! {"
        The hidden field marker.

        Fields with this marker will not be included in the output JSON when materialized.
      "},
      example: indoc! {r#"
        {a:: 3, b: 4}
      "#},
      outcome: Ok(()),
    }],
  },
  Token {
    text: "<<",
    purposes: &[TokenPurpose {
      doc: indoc! {"
        Bit-shift left.
      "},
      example: indoc! {r#"
        assert 1 << 2 == 4;
      "#},
      outcome: Ok(()),
    }],
  },
  Token {
    text: "<=",
    purposes: &[TokenPurpose {
      doc: indoc! {"
        Less than or equal to comparator.
      "},
      example: indoc! {r#"
        assert 1 <= 2;
        assert 2 <= 2;
      "#},
      outcome: Ok(()),
    }],
  },
  Token {
    text: "==",
    purposes: &[TokenPurpose {
      doc: indoc! {"
        Equality comparator.
      "},
      example: indoc! {r#"
        assert 2 + 2 == 4;
      "#},
      outcome: Ok(()),
    }],
  },
  Token {
    text: ">=",
    purposes: &[TokenPurpose {
      doc: indoc! {"
        Greater than or equal to comparator.
      "},
      example: indoc! {r#"
        assert 3 >= 2;
        assert 2 >= 2;
      "#},
      outcome: Ok(()),
    }],
  },
  Token {
    text: ">>",
    purposes: &[TokenPurpose {
      doc: indoc! {"
        Bit-shift right.
      "},
      example: indoc! {r#"
        assert 4 >> 2 == 1;
      "#},
      outcome: Ok(()),
    }],
  },
  Token {
    text: "||",
    purposes: &[TokenPurpose {
      doc: indoc! {"
        Logical or. Short-circuits.

        This does not error:
      "},
      example: indoc! {r#"
        assert 1 == 1 || (error "not evaluated");
      "#},
      outcome: Ok(()),
    }],
  },
  Token {
    text: "!",
    purposes: &[TokenPurpose {
      doc: indoc! {"
        Logical not.
      "},
      example: indoc! {r#"
        assert !false;
      "#},
      outcome: Ok(()),
    }],
  },
  Token {
    text: "$",
    purposes: &[TokenPurpose {
      doc: indoc! {"
        The root object.
      "},
      example: indoc! {r#"
        {
          a: 3,
          b: 4,
          c: {
            d: {
              e: $.a,
            },
          },
        }
      "#},
      outcome: Ok(()),
    }],
  },
  Token {
    text: "%",
    purposes: &[
      TokenPurpose {
        doc: indoc! {"
          Compute numerical modulus.
        "},
        example: indoc! {r#"
          assert 10 % 3 == 1;
        "#},
        outcome: Err(Error::Eval("not yet implemented: mod")),
      },
      TokenPurpose {
        doc: indoc! {"
          Format things into a string.
        "},
        example: indoc! {r#"
          assert "hi %s, how are you doing %s?" % ["there", "today"]
            == "hi there, how are you doing today?";
        "#},
        outcome: Err(Error::Eval("not yet implemented: mod")),
      },
    ],
  },
  Token {
    text: "&",
    purposes: &[TokenPurpose {
      doc: indoc! {"
        Bitwise and.

        Be careful with the relative precedence with `==` and other operators.
      "},
      example: indoc! {r#"
        assert (4 & 5) == 4;
      "#},
      outcome: Ok(()),
    }],
  },
  Token {
    text: "(",
    purposes: &[
      TokenPurpose {
        doc: indoc! {"
          Groups a sub-expression, overriding the default expression parsing precedence.
        "},
        example: indoc! {r#"
          assert 1 + 2 * 3 == 7;
          assert (1 + 2) * 3 == 9;
        "#},
        outcome: Ok(()),
      },
      TokenPurpose {
        doc: indoc! {"
          Begin a parameter or argument list in a function definition or call.
        "},
        example: indoc! {r#"
          local foo(x, y) = if x then y;
          assert foo(true, 4) == 4;
        "#},
        outcome: Ok(()),
      },
    ],
  },
  Token {
    text: ")",
    purposes: &[TokenPurpose {
      doc: indoc! {"
        The companion of `(`.
      "},
      example: indoc! {r#"
        assert 1 + 2 * 3 == 7;
        assert (1 + 2) * 3 == 9;

        local foo(x, y) = if x then y;
        assert foo(true, 4) == 4;
      "#},
      outcome: Ok(()),
    }],
  },
  Token {
    text: "*",
    purposes: &[TokenPurpose {
      doc: indoc! {"
        Numerical multiplication.
      "},
      example: indoc! {r#"
        assert 3 * 5 == 15;
      "#},
      outcome: Ok(()),
    }],
  },
  Token {
    text: "+",
    purposes: &[TokenPurpose {
      doc: indoc! {"
        Addition, for numbers, strings, arrays, or objects.
      "},
      example: indoc! {r#"
        local num = 1 + 2;
        local str = "hello, " + "world";
        local ary = [1, 2] + [8, 9];
        assert { a: -321, b: str } + { a: num, c: ary }
          == { a: 3, b: "hello, world", c: [1, 2, 8, 9] };
      "#},
      outcome: Err(Error::Eval("not yet implemented: object-object equality")),
    }],
  },
  Token {
    text: ",",
    purposes: &[TokenPurpose {
      doc: indoc! {"
        Separate elements in a parameter list, argument list, array, object, etc.
      "},
      example: indoc! {r#"
        local xs = [1, 2, 3];
        local ys = { a: 4, b: 6 };
        local f(a, b) = [a, b];
        f(ys, xs)
      "#},
      outcome: Ok(()),
    }],
  },
  Token {
    text: "-",
    purposes: &[TokenPurpose {
      doc: indoc! {"
        Numerical negation and subtraction.
      "},
      example: indoc! {r#"
        local negThree = -3;
        assert negThree - 2 == -5;
      "#},
      outcome: Ok(()),
    }],
  },
  Token {
    text: ".",
    purposes: &[TokenPurpose {
      doc: indoc! {"
        Get an object's field. Can be chained.
      "},
      example: indoc! {r#"
        local a = { foo: { bar: "quz" } };
        assert a.foo.bar == "quz";
      "#},
      outcome: Ok(()),
    }],
  },
  Token {
    text: "/",
    purposes: &[TokenPurpose {
      doc: indoc! {"
        Numerical division.
      "},
      example: indoc! {r#"
        assert 15 / 5 == 3;
      "#},
      outcome: Ok(()),
    }],
  },
  Token {
    text: ":",
    purposes: &[TokenPurpose {
      doc: indoc! {"
        The default field marker.
      "},
      example: indoc! {r#"
        { foo: 3, bar: false }
      "#},
      outcome: Ok(()),
    }],
  },
  Token {
    text: ";",
    purposes: &[TokenPurpose {
      doc: indoc! {"
        Separate a `local` or `assert` from the rest of the expression.
      "},
      example: indoc! {r#"
        local x = 3;
        assert x + 1 == 4;
        x + 5
      "#},
      outcome: Ok(()),
    }],
  },
  Token {
    text: "<",
    purposes: &[TokenPurpose {
      doc: indoc! {"
        Less than comparator.
      "},
      example: indoc! {r#"
        assert 4 < 5;
      "#},
      outcome: Ok(()),
    }],
  },
  Token {
    text: "=",
    purposes: &[TokenPurpose {
      doc: indoc! {"
        Assign an expression to a binding.
      "},
      example: indoc! {r#"
        local x = 4;
        assert x + 1 == 5;
      "#},
      outcome: Ok(()),
    }],
  },
  Token {
    text: ">",
    purposes: &[TokenPurpose {
      doc: indoc! {"
        Greater than comparator.
      "},
      example: indoc! {r#"
        assert 7 > 3;
      "#},
      outcome: Ok(()),
    }],
  },
  Token {
    text: "[",
    purposes: &[TokenPurpose {
      doc: indoc! {"
        Begin an array or array subscript.
      "},
      example: indoc! {r#"
        local xs = [1, 5, 7];
        assert xs[1] - 1 == 4;
      "#},
      outcome: Ok(()),
    }],
  },
  Token {
    text: "]",
    purposes: &[TokenPurpose {
      doc: indoc! {"
        The companion of `[`.
      "},
      example: indoc! {r#"
        local xs = [1, 5, 7];
        assert xs[1] - 1 == 4;
      "#},
      outcome: Ok(()),
    }],
  },
  Token {
    text: "^",
    purposes: &[TokenPurpose {
      doc: indoc! {"
        Bitwise xor.

        Be careful with the relative precedence with `==` and other operators.
      "},
      example: indoc! {r#"
        assert (2 ^ 8) == 10;
      "#},
      outcome: Ok(()),
    }],
  },
  Token {
    text: "{",
    purposes: &[TokenPurpose {
      doc: indoc! {"
        Begin an object or object comprehension.
      "},
      example: indoc! {r#"
        {a: 1, b: 2}
      "#},
      outcome: Ok(()),
    }],
  },
  Token {
    text: "|",
    purposes: &[TokenPurpose {
      doc: indoc! {"
        Bitwise or.

        Be careful with the relative precedence with `==` and other operators.
      "},
      example: indoc! {r#"
        assert (3 | 9) == 11;
      "#},
      outcome: Ok(()),
    }],
  },
  Token {
    text: "}",
    purposes: &[TokenPurpose {
      doc: indoc! {"
        The companion of `{`.
      "},
      example: indoc! {r#"
        {a: 1, b: 2}
      "#},
      outcome: Ok(()),
    }],
  },
  Token {
    text: "~",
    purposes: &[TokenPurpose {
      doc: indoc! {"
        Bitwise not.
      "},
      example: indoc! {r#"
        assert ~5 == -6;
      "#},
      outcome: Ok(()),
    }],
  },
  Token {
    text: "importbin",
    purposes: &[TokenPurpose {
      doc: indoc! {"
        Import a file as raw bytes.
      "},
      example: indoc! {r#"
        local f = importbin "foo.zip";
        assert std.length(f) == 1234;
      "#},
      outcome: Err(Error::PreEval("path not found: `foo.zip`")),
    }],
  },
  Token {
    text: "importstr",
    purposes: &[TokenPurpose {
      doc: indoc! {"
        Import a file as a string.
      "},
      example: indoc! {r#"
        local f = importstr "hi.txt";
        assert std.length(f) == 456;
      "#},
      outcome: Err(Error::PreEval("path not found: `hi.txt`")),
    }],
  },
  Token {
    text: "function",
    purposes: &[TokenPurpose {
      doc: indoc! {"
        Begin a function expression.
      "},
      example: indoc! {r#"
        std.map(function(x) x + 1, [3, 5])
      "#},
      outcome: Err(Error::Eval("not yet implemented: map")),
    }],
  },
  Token {
    text: "assert",
    purposes: &[
      TokenPurpose {
        doc: indoc! {"
          Begin an assert expression.
        "},
        example: indoc! {r#"
          assert 2 + 2 == 4;
          assert 1 < 2 : "one is smaller than two";
          3 + 3
        "#},
        outcome: Ok(()),
      },
      TokenPurpose {
        doc: indoc! {"
          Assert inside an object.
        "},
        example: indoc! {r#"
          {
            assert self.foo + self.foo == 4,
            foo: 2,
          }
        "#},
        outcome: Err(Error::StackOverflow),
      },
    ],
  },
  Token {
    text: "import",
    purposes: &[TokenPurpose {
      doc: indoc! {"
        Import a Jsonnet file.

        If `a.libsonnet` contains `1 + 2`, then this will evaluate to 7:
      "},
      example: indoc! {r#"
        local a = import "a.libsonnet";
        a + 4
      "#},
      outcome: Err(Error::PreEval("path not found: `a.libsonnet`")),
    }],
  },
  Token {
    text: "error",
    purposes: &[TokenPurpose {
      doc: indoc! {"
        Emit an error.
      "},
      example: indoc! {r#"
        local hm(x) = if x > 2 then x + 4 else error "oops";
        assert hm(3) == 7;
        hm(0)
      "#},
      outcome: Err(Error::Eval("explicit `error`: oops")),
    }],
  },
  Token {
    text: "false",
    purposes: &[TokenPurpose {
      doc: indoc! {"
        The boolean of logical falsity. Opposite of `true`.
      "},
      example: indoc! {r#"
        assert !false;
      "#},
      outcome: Ok(()),
    }],
  },
  Token {
    text: "local",
    purposes: &[TokenPurpose {
      doc: indoc! {"
        Create some bindings (usually just one).

        If more than one binding is provided, they may refer to each other.

        You can also use syntax sugar with `local` when binding a function to a variable,
        instead of using the `function` keyword.
      "},
      example: indoc! {r#"
        local three = 1 + 2;
        assert three + 4 == 7;

        local f(a, b=1) = a + b;
        assert f(2) == 3;
      "#},
      outcome: Ok(()),
    }],
  },
  Token {
    text: "super",
    purposes: &[TokenPurpose {
      doc: indoc! {"
        The parent (left) object in a chain of objects connected together with `+`.
      "},
      example: indoc! {r#"
        assert { a: 3 } + { b: super.a + 1 }
          == { a: 3, b: 4 };
      "#},
      outcome: Err(Error::Eval("not yet implemented: object-object equality")),
    }],
  },
  Token {
    text: "else",
    purposes: &[TokenPurpose {
      doc: indoc! {"
        Denotes what an `if` expression evaluates to if the condition is `false`.
      "},
      example: indoc! {r#"
        assert (if 1 > 2 then 3 else 4) == 4;
      "#},
      outcome: Ok(()),
    }],
  },
  Token {
    text: "null",
    purposes: &[TokenPurpose {
      doc: indoc! {"
        A value representing nothing, unknown, not present, etc.
      "},
      example: indoc! {r#"
        { name: "guy incognito", location: null }
      "#},
      outcome: Ok(()),
    }],
  },
  Token {
    text: "self",
    purposes: &[TokenPurpose {
      doc: indoc! {"
        The current object.
      "},
      example: indoc! {r#"
        {
          a: 3,
          b: self.a + 1,
        }
      "#},
      outcome: Ok(()),
    }],
  },
  Token {
    text: "then",
    purposes: &[TokenPurpose {
      doc: indoc! {"
        Denotes what an `if` expression evaluates to if the condition is `true`.
      "},
      example: indoc! {r#"
        assert (if 1 < 2 then 3 else 4) == 3;
      "#},
      outcome: Ok(()),
    }],
  },
  Token {
    text: "true",
    purposes: &[TokenPurpose {
      doc: indoc! {"
        The boolean of logical truth. Opposite of `false`.
      "},
      example: indoc! {r#"
        assert true;
      "#},
      outcome: Ok(()),
    }],
  },
  Token {
    text: "for",
    purposes: &[TokenPurpose {
      doc: indoc! {"
        Loop over an array to create an array or object via a comprehension.
      "},
      example: indoc! {r#"
        local addOne(xs) = [x + 1 for x in xs];
        assert addOne([2, 5]) == [3, 6];

        local lengths(xs) = { [x]: std.length(x) for x in xs };
        assert lengths(["foo", "hi"])
          == { foo: 3, hi: 2 };
      "#},
      outcome: Err(Error::Eval("not yet implemented: makeArray")),
    }],
  },
  Token {
    text: "if",
    purposes: &[TokenPurpose {
      doc: indoc! {"
        Branch on a boolean with `then` and `else`.

        You can skip the `else`. It defaults to `else null`.
      "},
      example: indoc! {r#"
        local hm(x) = if x then 4 else 5;
        assert hm(true) == 4;
        assert hm(false) == 5;

        local huh(x) = if x then 4;
        assert huh(true) == 4;
        assert huh(false) == null;
      "#},
      outcome: Ok(()),
    }],
  },
  Token {
    text: "in",
    purposes: &[
      TokenPurpose {
        doc: indoc! {"
          Denotes the thing to loop over in a `for`.
        "},
        example: indoc! {r#"
          local xs = [1, 3];
          assert [x + 1 for x in xs]
            == [2, 4];
        "#},
        outcome: Err(Error::Eval("not yet implemented: makeArray")),
      },
      TokenPurpose {
        doc: indoc! {"
          Tests for field membership in objects.
        "},
        example: indoc! {r#"
          assert "foo" in { foo: 3 };
          assert !("bar" in { foo: 3 });
        "#},
        outcome: Ok(()),
      },
    ],
  },
];
