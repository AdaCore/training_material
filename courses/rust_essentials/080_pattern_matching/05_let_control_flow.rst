==================
Let Control Flow
==================

--------------------
"let" as a Pattern
--------------------

- Every :rust:`let` binding uses a pattern

- Simple bindings always match

- More complex patterns may fail to match

.. code:: rust

   let x = 5;
   let (a, b) = (1, 2);

----------------------
Conditional Matching
----------------------

- Some patterns only match certain values

- :rust:`match` can always be used to handle this

- Rust provides shorthand forms for common cases

----------
"if let"
----------

- Matches a single pattern conditionally

- Executes only when the pattern matches

- All other cases are ignored

.. code:: rust

   let value = Some(3);

   if let Some(x) = value {
       println!("x = {}", x);
   }

---------------------
"if let" vs "match"
---------------------

- :rust:`if let` is shorthand for a two-arm :rust:`match`

- Use when only one case is interesting

- Prefer `match` when handling multiple cases

.. code:: rust

   // equivalent to the previous example
   match value {
       Some(x) => println!("x = {}", x),
       _ => {}
   }

-------------
"while let"
-------------

- Repeats while a pattern continues to match

- Commonly used for iterative extraction

- Stops when the pattern no longer matches

.. code:: rust

   let mut value = Some(3);

   while let Some(x) = value {
       println!("x = {}", x);
       value = None;
   }

---------------------------
Pattern-Based Convenience
---------------------------

- :rust:`let`, :rust:`if let`, and :rust:`while let` all use patterns

- These forms reduce boilerplate for common matches

- The same pattern rules apply everywhere
