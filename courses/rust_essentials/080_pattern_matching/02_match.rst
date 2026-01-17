=======
Match
=======

-----------------
What Is "match"
-----------------

- Compares a value against a set of patterns

- At least one pattern must match

  - Only one match is allowed
  - First one wins!

- Selected arm determines the result

.. note::

  :rust:`match` is an expression, not a statement

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

  match n {
    1 => println!("Single scoop!"),
    2 => println!("Double scoop!!"),
    _ => println!("Wow, that's a lot of ice cream!"),
  }

----------------
Exhaustiveness
----------------

- All possible cases must be handled

- Missing cases cause a compile-time error

- This applies to all :rust:`match` expressions

.. code:: rust

  enum Direction {
    North,
    South,
    East,
    West,
  }

  let travel_to = Direction::East;

  // ERROR: non-exhaustive patterns: `East` and `West` not covered
  match travel_to {
    Direction::North => println!("Heading Up"),
    Direction::South => println!("Heading Down"),
  }

------------------------
Matching with Bindings
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

- The first matching arm is selected

---------------------
Why "match" Matters
---------------------

- Makes all cases explicit

- Each arm is independent and exclusive (no fallthrough between cases)

- Enables compiler-checked completeness

.. note::

  :rust:`match` is central to how Rust models branching logic
