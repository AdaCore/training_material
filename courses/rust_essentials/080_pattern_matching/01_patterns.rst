==========
Patterns
==========

--------------------
What Is a Pattern?
--------------------

- Core language feature of Rust

- Describes the **structure** of a value

- Used to *test* and *decompose* values

- Successful matching may introduce new bindings

**Basic Pattern**

.. code:: rust

  let musketeers = 3;

  // Pattern:   Identifier (musketeers)
  // Structure: Single, scalar value
  // Binding:   'musketeers' is now bound to value '3'

--------------------------
Patterns Are Declarative
--------------------------

- Patterns describe *what* (shape) rather than *how* (steps)

- Matching is a structural check

  - Not a sequence of procedural field accesses

- Scale effectively as data structures grow in complexity

.. code:: rust

  // Declarative: Describing the 'stencil'
  match point {
    Point { x: 0, y: ver } => println!("On Y axis at {ver}"),
    _ => {}
  }

  // Procedural: Step-by-step instructions (what patterns avoid)
  if point.x == 0 {
    let ver = point.y;
    println!("On Y axis at {ver}");
  }

----------------------
Patterns as Bindings
----------------------

- Every :rust:`let` binding uses a pattern

- Simple bindings use identifier patterns

- Complex patterns can destructure values

.. code:: rust

  let number = 5;                // Identifier pattern

  let (first, second) = (1, 2);  // Tuple pattern

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
  :number-lines: 1

  let point = (0, 5);

  match point {
    (0, y) => println!("on y-axis at {}", y),
    _ => {}
  }

:command:`on y-axis at 5`

- If line 1 was :rust:`let point = (5, 0);`

  - Fails to match the first pattern because 5 does not equal 0
  - Matches the wildcard pattern (:rust:`_`)

    - Performs no action
    - Nothing is printed

--------------------
Pattern Vocabulary
--------------------

- **Literal Matching:** Patterns can match specific values like numbers or strings

- **Alternative Patterns:** Use the "pipe" (:rust:`|`) to handle multiple values in a single arm

- **Variable Bindings:** Use :rust:`@` to give a name to a value while checking it against a range or pattern

- **The Rest Pattern:** A placeholder (:rust:`..`) that ignores "everything else" in a sequence or structure

.. code:: rust

  let x = 5;

  match x {
      // Matches 1, 2, or 3
      1 | 2 | 3 => println!("Small"),
      
      // Binds the value to 'n' AND checks the range
      n @ 4..=10 => println!("Value {n} is in range"),
      
      _ => println!("Other"),
  }

-----------------------------
Patterns in Rust Constructs
-----------------------------

Patterns are reused consistently across the language

- :rust:`let` bindings

- :rust:`match` expressions

- :rust:`if let` and :rust:`while let`

- function parameters (covered in another module)
