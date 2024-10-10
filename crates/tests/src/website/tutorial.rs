//! Examples from the [tutorial](https://jsonnet.org/learning/tutorial.html).

use crate::check::JsonnetInput;

#[test]
fn t1() {
  JsonnetInput::manifest(
    r#"
/* A C-style comment. */
# A Python-style comment.
{
  cocktails: {
    // Ingredient quantities are in fl oz.
    'Tom Collins': {
      ingredients: [
        { kind: "Farmer's Gin", qty: 1.5 },
        { kind: 'Lemon', qty: 1 },
        { kind: 'Simple Syrup', qty: 0.5 },
        { kind: 'Soda', qty: 2 },
        { kind: 'Angostura', qty: 'dash' },
      ],
      garnish: 'Maraschino Cherry',
      served: 'Tall',
      description: |||
        The Tom Collins is essentially gin and
        lemonade.  The bitters add complexity.
      |||,
    },
    Manhattan: {
      ingredients: [
        { kind: 'Rye', qty: 2.5 },
        { kind: 'Sweet Red Vermouth', qty: 1 },
        { kind: 'Angostura', qty: 'dash' },
      ],
      garnish: 'Maraschino Cherry',
      served: 'Straight Up',
      description: @'A clear \ red drink.',
    },
  },
}
"#,
    r#"
{
  "cocktails": {
    "Manhattan": {
      "description": "A clear \\ red drink.",
      "garnish": "Maraschino Cherry",
      "ingredients": [
        {
          "kind": "Rye",
          "qty": 2.5
        },
        {
          "kind": "Sweet Red Vermouth",
          "qty": 1
        },
        {
          "kind": "Angostura",
          "qty": "dash"
        }
      ],
      "served": "Straight Up"
    },
    "Tom Collins": {
      "description": "The Tom Collins is essentially gin and\nlemonade.  The bitters add complexity.\n",
      "garnish": "Maraschino Cherry",
      "ingredients": [
        {
          "kind": "Farmer's Gin",
          "qty": 1.5
        },
        {
          "kind": "Lemon",
          "qty": 1
        },
        {
          "kind": "Simple Syrup",
          "qty": 0.5
        },
        {
          "kind": "Soda",
          "qty": 2
        },
        {
          "kind": "Angostura",
          "qty": "dash"
        }
      ],
      "served": "Tall"
    }
  }
}
"#,
  ).check();
}

#[test]
fn t2() {
  JsonnetInput::manifest(
    r"
// A regular definition.
local house_rum = 'Banks Rum';

{
  // A definition next to fields.
  local pour = 1.5,

  Daiquiri: {
    ingredients: [
      { kind: house_rum, qty: pour },
      { kind: 'Lime', qty: 1 },
      { kind: 'Simple Syrup', qty: 0.5 },
    ],
    served: 'Straight Up',
  },
  Mojito: {
    ingredients: [
      {
        kind: 'Mint',
        action: 'muddle',
        qty: 6,
        unit: 'leaves',
      },
      { kind: house_rum, qty: pour },
      { kind: 'Lime', qty: 0.5 },
      { kind: 'Simple Syrup', qty: 0.5 },
      { kind: 'Soda', qty: 3 },
    ],
    garnish: 'Lime wedge',
    served: 'Over crushed ice',
  },
}
",
    r#"
{
  "Daiquiri": {
    "ingredients": [
      {
        "kind": "Banks Rum",
        "qty": 1.5
      },
      {
        "kind": "Lime",
        "qty": 1
      },
      {
        "kind": "Simple Syrup",
        "qty": 0.5
      }
    ],
    "served": "Straight Up"
  },
  "Mojito": {
    "garnish": "Lime wedge",
    "ingredients": [
      {
        "action": "muddle",
        "kind": "Mint",
        "qty": 6,
        "unit": "leaves"
      },
      {
        "kind": "Banks Rum",
        "qty": 1.5
      },
      {
        "kind": "Lime",
        "qty": 0.5
      },
      {
        "kind": "Simple Syrup",
        "qty": 0.5
      },
      {
        "kind": "Soda",
        "qty": 3
      }
    ],
    "served": "Over crushed ice"
  }
}
"#,
  )
  .check();
}

#[test]
fn t3() {
  JsonnetInput::manifest(
    r#"
{
  'Tom Collins': {
    ingredients: [
      { kind: "Farmer's Gin", qty: 1.5 },
      { kind: 'Lemon', qty: 1 },
      { kind: 'Simple Syrup', qty: 0.5 },
      { kind: 'Soda', qty: 2 },
      { kind: 'Angostura', qty: 'dash' },
    ],
    garnish: 'Maraschino Cherry',
    served: 'Tall',
  },
  Martini: {
    ingredients: [
      {
        // Use the same gin as the Tom Collins.
        kind:
          $['Tom Collins'].ingredients[0].kind,
        qty: 2,
      },
      { kind: 'Dry White Vermouth', qty: 1 },
    ],
    garnish: 'Olive',
    served: 'Straight Up',
  },
  // Create an alias.
  'Gin Martini': self.Martini,
}
"#,
    r#"
{
  "Gin Martini": {
    "garnish": "Olive",
    "ingredients": [
      {
        "kind": "Farmer's Gin",
        "qty": 2
      },
      {
        "kind": "Dry White Vermouth",
        "qty": 1
      }
    ],
    "served": "Straight Up"
  },
  "Martini": {
    "garnish": "Olive",
    "ingredients": [
      {
        "kind": "Farmer's Gin",
        "qty": 2
      },
      {
        "kind": "Dry White Vermouth",
        "qty": 1
      }
    ],
    "served": "Straight Up"
  },
  "Tom Collins": {
    "garnish": "Maraschino Cherry",
    "ingredients": [
      {
        "kind": "Farmer's Gin",
        "qty": 1.5
      },
      {
        "kind": "Lemon",
        "qty": 1
      },
      {
        "kind": "Simple Syrup",
        "qty": 0.5
      },
      {
        "kind": "Soda",
        "qty": 2
      },
      {
        "kind": "Angostura",
        "qty": "dash"
      }
    ],
    "served": "Tall"
  }
}
"#,
  )
  .check();
}

#[test]
fn t4() {
  JsonnetInput::manifest(
    r#"
{
  Martini: {
    local drink = self,
    ingredients: [
      { kind: "Farmer's Gin", qty: 1 },
      {
        kind: 'Dry White Vermouth',
        qty: drink.ingredients[0].qty,
      },
    ],
    garnish: 'Olive',
    served: 'Straight Up',
  },
}
"#,
    r#"
{
  "Martini": {
    "garnish": "Olive",
    "ingredients": [
      {
        "kind": "Farmer's Gin",
        "qty": 1
      },
      {
        "kind": "Dry White Vermouth",
        "qty": 1
      }
    ],
    "served": "Straight Up"
  }
}
"#,
  )
  .check();
}

#[test]
#[should_panic = "not yet implemented: object-object equality"]
fn t5() {
  JsonnetInput::manifest(
    r"
// silence type errors
local blackBox(x) = x;

{
  concat_array: [1, 2, 3] + [4],
  concat_string: '123' + 4,
  equality1: 1 == blackBox('1'),
  equality2: [{}, { x: 3 - 1 }]
             == [{}, { x: 2 }],
  ex1: 1 + 2 * 3 / (4 + 5),
  // Bitwise operations first cast to int.
  ex2: self.ex1 | 3,
  // Modulo operator.
  ex3: self.ex1 % 2,
  // Boolean logic
  ex4: (4 > 3) && (1 <= 3) || false,
  // Mixing objects together
  obj: { a: 1, b: 2 } + { b: 3, c: 4 },
  // Test if a field is in an object
  obj_member: 'foo' in { foo: 1 },
  // String formatting
  str1: 'The value of self.ex2 is '
        + self.ex2 + '.',
  str2: 'The value of self.ex2 is %g.'
        % self.ex2,
  str3: 'ex1=%0.2f, ex2=%0.2f'
        % [self.ex1, self.ex2],
  // By passing self, we allow ex1 and ex2 to
  // be extracted internally.
  str4: 'ex1=%(ex1)0.2f, ex2=%(ex2)0.2f'
        % self,
  // Do textual templating of entire files:
  str5: |||
    ex1=%(ex1)0.2f
    ex2=%(ex2)0.2f
  ||| % self,
}",
    r#"
{
  "concat_array": [
    1,
    2,
    3,
    4
  ],
  "concat_string": "1234",
  "equality1": false,
  "equality2": true,
  "ex1": 1.6666666666666665,
  "ex2": 3,
  "ex3": 1.6666666666666665,
  "ex4": true,
  "obj": {
    "a": 1,
    "b": 3,
    "c": 4
  },
  "obj_member": true,
  "str1": "The value of self.ex2 is 3.",
  "str2": "The value of self.ex2 is 3.",
  "str3": "ex1=1.67, ex2=3.00",
  "str4": "ex1=1.67, ex2=3.00",
  "str5": "ex1=1.67\nex2=3.00\n"
}
"#,
  )
  .check();
}
