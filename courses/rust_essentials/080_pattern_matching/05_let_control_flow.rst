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

  // ALWAYS matches. This is "irrefutable"
  let x = 5;

  // ERROR. This "refutable"
  let 7 = x;

:error:`error[E0005]: refutable pattern in local binding`

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

- Guard runs *after* the pattern matches

  - ...but *before* the arm's code block

- Guards are *dynamic*

  - Compiler usually requires a "catch-all" arm (:rust:`_`)

.. code:: rust

  let pair = (2, -2);

  match pair {
      (x, y) if x == y => println!("They match!"),
      (x, y) if x + y == 0 => println!("They neutralize!"),
      (x, _) if x % 2 == 0 => println!("The first is even"),
      _ => println!("No special relationship"),
  }

:command:`They neutralize!`

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

:command:`x = 3`

----------------------
"if let" vs. "match"
----------------------

- :rust:`if let` is shorthand for a two-arm :rust:`match`

- Suited for logic where only one specific pattern is relevant

- :rust:`match` remains the standard for handling multiple distinct cases

.. code:: rust

  let value = Some(3);

  // Shorthand version:
  if let Some(x) = value {
    println!("x = {}", x);
  }

:command:`x = 3`

  // Equivalent 'match' version:
  match value {
    Some(x) => println!("x = {}", x),
    _ => {} // Explicitly ignore all other cases
  }

:command:`x = 3`

-------------
"while let"
-------------

- Commonly used for iterative extraction

- Repeats while a pattern continues to match

- Stops when the pattern no longer matches

.. code:: rust

  enum Progress {
        Step(i32),
        Done,
    }

    let mut current = Progress::Step(3);

    // Loop continues as long as 'current' matches 'Step(val)'
    while let Progress::Step(val) = current {
        println!("Steps remaining: {val}");
        
        if val > 0 {
            current = Progress::Step(val - 1);
        } else {
            current = Progress::Done; // Pattern will fail on next check
        }
    }

    println!("Finished!");
  
:command:`Steps remaining: 3`
:command:`Steps remaining: 2`
:command:`Steps remaining: 1`
:command:`Steps remaining: 0`
:command:`Finished!`

---------------------------
Pattern-Based Convenience
---------------------------

- :rust:`let`, :rust:`if let`, and :rust:`while let` all use patterns

  - Reduce boilerplate for common matches

- Same pattern rules apply everywhere
