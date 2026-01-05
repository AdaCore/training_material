=======
Match
=======

-----------------
What Is "match"
-----------------

- :rust:`match` compares a value against a set of patterns

- Exactly one pattern must match

- The selected arm determines the result

.. note::

   :rust:`match` is an expression, not a statement.

--------------------------
"match" as an Expression
--------------------------

- Every :rust:`match` evaluates to a value

- All arms must produce compatible types

- The result can be bound to a name

.. code:: rust

   let x = 2;

   let description = match x {
       0 => "zero",
       1 => "one",
       _ => "many",
   };

------------
Match Arms
------------

- Each arm consists of a pattern and an expression

- Patterns are tested top to bottom

- The first matching arm is selected

.. code:: rust

   let n = 5;

   match n {
       1 => println!("one"),
       2 => println!("two"),
       _ => println!("other"),
   }

----------------
Exhaustiveness
----------------

- All possible cases must be handled

- Missing cases cause a compile-time error

- This applies to all :rust:`match` expressions

------------------
Wildcard Pattern
------------------

- :rust:`_` matches any remaining cases

- Commonly used as a catch-all

- Does not bind the matched value

.. code:: rust

   let n = 10;

   match n {
       0 => println!("zero"),
       _ => println!("non-zero"),
   }

------------------------
Matching with Bindings
------------------------

- Patterns can bind values while matching

- Bound names are available in the arm body

- Useful for extracting data inline

.. code:: rust

   let value = Some(4);

   match value {
       Some(x) => println!("x = {}", x),
       None => println!("no value"),
   }

-----------------
Nested Patterns
-----------------

- Patterns can be composed hierarchically

- Matching proceeds from outer to inner structure

- Enables concise handling of structured data

.. code:: rust

   let point = (0, 3);

   match point {
       (0, y) => println!("on y-axis at {}", y),
       (x, 0) => println!("on x-axis at {}", x),
       (x, y) => println!("({}, {})", x, y),
   }

---------------------
Why "match" Matters
---------------------

- Makes all cases explicit

- Eliminates implicit fallthrough

- Enables compiler-checked completeness

.. note::

   :rust:`match` is central to how Rust models branching logic
