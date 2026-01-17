==========
Patterns
==========

-------------------
What Is a Pattern
-------------------

- Core language feature of Rust

- Describes the **structure** of a value

- Used to *test* and *decompose* values

- Successful matching may introduce new bindings

**Basic Pattern**

.. code:: rust
    
    // Pattern: musketeers (identifier)
    // Structure: Single, scalar value
    // Binding: "musketeers" is now bound to value "3"

    let musketeers = 3;

--------------------------
Patterns Are Declarative
--------------------------

- Patterns describe the *what* (shape) rather than the *how* (steps)

- Matching is a structural check, not a sequence of procedural field accesses

- This style scales effectively as data structures grow in complexity

.. code:: rust

   // Declarative: Describing the "stencil"
   match point {
       Point { x: 0, y: vertical } => println!("On Y axis at {vertical}"),
       _ => {}
   }

   // Procedural: Step-by-step instructions (What patterns avoid)
   if point.x == 0 {
       let vertical = point.y;
       println!("On Y axis at {vertical}");
   }

----------------------
Patterns as Bindings
----------------------

- Every :rust:`let` binding uses a pattern

- Simple bindings use identifier patterns

- Complex patterns can destructure values

.. code:: rust

   let number = 5;                // identifier pattern

   let (first, second) = (1, 2);  // tuple pattern

------------------
Literal Patterns
------------------

- Match exact values

- Commonly used in :rust:`match` expressions

- Useful for branching on specific cases

.. code:: rust

   let choices = 3;

   match choices {
       0 => println!("zero"),
       1 => println!("one"),
       _ => println!("too many"),
   }

------------------
Wildcard Pattern
------------------

- :rust:`_` matches any value

- Does not *bind* or *move* the value

- Often used to ignore irrelevant cases

.. code:: rust

   enum Status {
       Ok(i32),
       Error,
   }

   let status = Status::Ok(10);

   match status {
       Status::Ok(_) => println!("ok"),
       Status::Error => println!("error"),
   }

-----------------------
Binding with Patterns
-----------------------

- Identifier patterns bind matched values to names

- Bindings only exist when the pattern matches

- Commonly used with enums and tuples

.. code:: rust

    let (first, second) = (10, 20);

    println!("first is {}, second is {}", first, second);

---------------------
Pattern Composition
---------------------

- Patterns may be composed recursively

- Larger patterns are built from smaller ones

- Inner patterns describe substructure

.. code:: rust

   let point = (0, 5);

   match point {
       (0, y) => println!("on y-axis at {}", y),
       _ => {}
   }

:command:`on y-axis at 5`

- If the value was :rust:`let point = (5, 0);`

    - Fails to match the first pattern because 5 does not equal 0
    - Matches the wildcard pattern (:rust:`_`)

        - Performs no action
        - Nothing is printed

-----------------------------
Patterns in Rust Constructs
-----------------------------

Patterns are reused consistently across the language

- :rust:`let` bindings

- :rust:`match` expressions

- :rust:`if let` and :rust:`while let`

- function parameters (covered in another module)
