=======
Match
=======

------------------
What Is "match"?
------------------

- Compares a value against a set of patterns

- At least one pattern must match

  - First matching arm is executed

- Selected arm determines the result

.. note::

  :rust:`match` is an *expression*, not a *statement*

--------------------------
"match" as an Expression
--------------------------

- Every :rust:`match` evaluates to a value

- All arms must produce compatible types

- Result can be bound to a name

.. code:: rust

  let choices = 2;

  let description = match choices {
    0 => "zero",
    1 => "one",
    _ => "too many",
  };

------------
Match Arms
------------

- Each arm consists of a pattern and an expression

- Patterns are tested top to bottom

- First matching arm is selected

.. code:: rust

  let scoops  = 5;

  match scoops {
    1 => println!("Single scoop!"),
    2 => println!("Double scoop!!"),
    _ => println!("Wow, that's a lot of ice cream!"),
  }

:command:`Wow, that's a lot of ice cream!`
----------------
Exhaustiveness
----------------

- All possible cases must be handled

- Missing cases cause a *compile-time* error

- Applies to all :rust:`match` expressions

.. code:: rust

  enum Direction {
    North,
    South,
    East,
    West,
  }

  let travel_to = Direction::East;

  match travel_to {
    Direction::North => println!("Heading Up"),
    Direction::South => println!("Heading Down"),
  }

.. container:: latex_environment scriptsize

  :error:`error[E0004]: non-exhaustive patterns: 'Direction::East' and 'Direction::West' not covered`

------------------------
Matching With Bindings
------------------------

- Patterns can bind values while matching

- Bound names are available in the arm body

- :rust:`_` ignores the matched value

.. code:: rust

  let player_score = (10, 250);

  match player_score {
    (10, bonus) => println!("Level 10! Bonus: {}", bonus),
    _ => println!("Keep playing!"),
  }

:command:`Level 10! Bonus: 250`

----------------------------
Nested Patterns in "match"
----------------------------

- Nested patterns are checked within the selected arm

- Rust does not choose the "most specific" pattern

.. code:: rust

  let point = (0, 0);

  match point {
    (0, y) => println!("on y-axis at {}", y),
    (x, 0) => println!("on x-axis at {}", x),
    (x, y) => println!("({}, {})", x, y),
  }

:command:`on y-axis at 0`

.. note::

  First matching arm is selected

---------------------
Why "match" Matters
---------------------

- Makes all cases explicit

- Each arm is independent and exclusive

  - No fallthrough between cases

- Enables compiler-checked completeness

.. note::

  :rust:`match` is central to how Rust models branching logic
