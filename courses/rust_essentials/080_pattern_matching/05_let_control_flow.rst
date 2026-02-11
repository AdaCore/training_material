==================
"Let" Control Flow
==================

--------------------
"let" as a Pattern
--------------------

- Every :rust:`let` binding uses a pattern

- Simple bindings always match

- Complex patterns may fail to match

.. code:: rust

  let x = 5; // ALWAYS matches. This is "irrefutable"

  // ERROR: refutable pattern in local binding
  // This pattern ONLY matches if 'x' is exactly '7'
  let 7 = x;

----------------------
Conditional Matching
----------------------

- Some patterns only match certain values

- :rust:`match` can always be used to handle this

- Rust provides shorthand forms for common cases

--------------
Match Guards
--------------

- Use an :rust:`if` condition to filter a pattern based on its value

- Guard runs *after* the pattern matches, but *before* the arm's code block

- Guards are dynamic

  - Compiler usually requires a "catch-all" arm (:rust:`_`)

.. code:: rust

  let pair = (2, -2);

  match pair {
      (x, y) if x == y => println!("They match!"),
      (x, y) if x + y == 0 => println!("They neutralize!"),
      (x, _) if x % 2 == 0 => println!("The first is even"),
      _ => println!("No special relationship"),
  }

----------
"if let"
----------

- Matches a single pattern conditionally

- Follows assignment order

  - Pattern = value
  - Cannot be swapped

- All other cases are implicitly ignored

.. code:: rust

  let value = Some(3);

  if let Some(x) = value {
    println!("x = {}", x);
  }

---------------------
"if let" vs "match"
---------------------

- :rust:`if let` is shorthand for a two-arm :rust:`match`

- Suited for logic where only one specific pattern is relevant

- :rust:`match` remains the standard for handling multiple distinct cases

.. code:: rust

  let value = Some(3);

  // Shorthand version:
  if let Some(x) = value {
    println!("x = {}", x);
  }

  // Equivalent "match" version:
  match value {
    Some(x) => println!("x = {}", x),
    _ => {} // Explicitly ignore all other cases
  }

-------------
"while let"
-------------

- Commonly used for iterative extraction

- Repeats while a pattern continues to match

- Stops when the pattern no longer matches

.. code:: rust

  let mut energy_level = Some(3);

  // While the battery still has 'Some' charge...
  while let Some(percent) = energy_level {
    if percent > 0 {
      println!("Device running... {}% remaining", percent);
      energy_level = Some(percent - 1);
    } else {
      println!("Shutting down...");
      energy_level = None; // Terminal state: Loop exits
    }
  }

---------------------------
Pattern-Based Convenience
---------------------------

- :rust:`let`, :rust:`if let`, and :rust:`while let` all use patterns

  - Reduce boilerplate for common matches

- Same pattern rules apply everywhere
