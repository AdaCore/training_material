=======================
Destructuring Structs
=======================

-----------------
Struct Patterns
-----------------

- Match values by field name

- Fields can be accessed without dot notation

- Patterns may *bind*, *ignore*, or *partially match* fields

.. tip::

  Destructuring works anywhere patterns are allowed

-------------------------------------
Destructuring: The Stencil Metaphor
-------------------------------------

- Think of a **pattern** as a *stencil* placed over a value

- **Match (left of colon):** tells Rust *where* to look inside the struct

- **Binding (right of colon):** where the data "falls through" into your local scope

- **Result:** You "break open" the complex struct to get exactly the pieces you need

.. code:: rust

  // Placing the stencil (pattern) over the point 'p'
  let Point { x, y } = p;

---------------------
Basic Destructuring
---------------------

- The pattern mirrors the struct's shape to extract values

- **Order Independence:** Rust matches by field name, not position

- **Implicit Matching:** Patterns work anywhere a variable is introduced

.. code:: rust

  let p = Point { x: 3, y: 4 };

  // Pattern shape must match the struct shape
  let Point { x, y } = p; 

  // Fields are found by name, so order doesn't matter
  let Point { y, x } = p;

-------------------
Shorthand Binding
-------------------

- Field names can be reused as bindings

- Reduces repetition for common cases

- Shorthand is a syntactic shortcut for longhand renaming

- Most common way to destructure in Rust

.. code:: rust

  // Shorthand
  let Point { x, y } = p;

  // ...is equivalent to explicit "rename" syntax:
  let Point { x: x, y: y } = p;

-----------------------
Longhand and Renaming
-----------------------

- Use :rust:`field: variable` to rename data as it is extracted

- Useful when generic field names (like :rust:`x`) need descriptive local names

- New variables inherit the exact type (e.g., :rust:`i32`) from the struct

.. code:: rust

  struct Point {
    x: i32,
    y: i32,
  }

  // Renaming 'x' to 'new_x' to provide local context
  // Types are strictly preserved during the "binding"
  let Point { x: new_x, y: new_y } = p; 

  // new_x: i32 = 3
  // new_y: i32 = 4

-----------------
Ignoring Fields
-----------------

- Unused fields may be ignored

- :rust:`..` matches remaining fields

- Useful when only part of a struct matters

.. container:: latex_environment scriptsize

  .. code:: rust

    struct PhysicsObject {
      id: u32,
      x: i32,
      y: i32,
      velocity: f64,
    }

    let obj = PhysicsObject { id: 1, x: 10, y: 20, velocity: 5.5 };

    // Capture 'x' and ignore all other fields
    let PhysicsObject { x, .. } = obj;

    // Ignore a specific field by name using '_'
    let PhysicsObject { id, velocity: _, .. } = obj;

    // Capturing multiple specific fields
    let PhysicsObject { id, velocity, .. } = obj;

.. note::

  :rust:`..` *captures* specific named fields and *ignores* the rest
