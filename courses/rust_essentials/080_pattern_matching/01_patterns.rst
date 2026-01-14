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

- Literal patterns match exact values

- Commonly used in :rust:`match` expressions

- Useful for branching on specific cases

.. code:: rust

   let n = 3;

   match n {
       0 => println!("zero"),
       1 => println!("one"),
       _ => println!("many"),
   }

------------------
Wildcard Pattern
------------------

- :rust:`_` matches any value

- Does not bind or move the value

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

   let opt = Some(3);

   match opt {
       Some(x) => println!("x = {}", x),
       None => println!("no value"),
   }

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

-----------------------------
Patterns in Rust Constructs
-----------------------------

- Patterns are reused consistently across the language

- :rust:`let` bindings

- :rust:`match` expressions

- :rust:`if let` and :rust:`while let`

- function parameters (later)
