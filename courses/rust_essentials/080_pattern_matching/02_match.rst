=======
Match
=======

--------------------------------
Refutable Patterns and "match"
--------------------------------

- **Refutable Pattern** - might not match the data (e.g., :rust:`Some` vs :rust:`None`)

- :rust:`match` keyword allows comparison of a value against one or more refutable patterns

- :rust:`match` arms must cover **every single possibility**

- Use :rust:`_` as a "catch-all" pattern for remaining cases

------------------------
Anatomy of a Match Arm
------------------------

- Use :rust:`|` to match several values in one arm

- Use :rust:`..=` to match an **inclusive** range of values

- Patterns can bind parts of a value to a name for use inside the arm (i.e., a variable binding)

.. code:: rust

   match input {
      // Literal
      'q' => println!("Quitting"),
      // Range
      '0'..='9' => println!("Number"),
      // Binding + Guard
      key if key.is_lowercase() => println!("{key}"),
      // Wildcard
      _ => println!("Something else"),
   }

--------------
Match Guards
--------------

- :dfn:`Match Guard` - an additional :rust:`if` condition placed after a pattern

- Arm only matches if the pattern matches **and** the guard condition is **true**

- If guard is **false**, match continues to check subsequent arms

.. note::

   Note the same as an :rust:`if` inside the branch; failing a guard lets other arms
   be considered
