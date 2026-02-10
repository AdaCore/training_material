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

- **Mapping (Longhand):** Explicitly rename data using :rust:`field: variable`

- **Order Independence:** Rust matches by field name, not position

- **Shorthand:** Use when the *variable name* matches the *field name*

.. code:: rust

  struct Point {
    x: i32,
    y: i32,
  }

  let p = Point { x: 3, y: 4 };

  // Longhand: Renaming fields to new variables
  let Point { x: new_x, y: new_y } = p;
  // new_x = 3, new_y = 4

  // Shorthand: Variable names match field names
  let Point { x, y } = p;
  // x = 3, y = 4

  // Order independence: 'y' found by name
  let Point { y: pos_y, x: pos_x } = p;

-------------------
Shorthand Binding
-------------------

- Field names can be reused as bindings

- Reduces repetition for common cases

- Shorthand is a syntactic shortcut for longhand renaming

.. code:: rust

  // Shorthand
  let Point { x, y } = p;

  // Above is equivalent to explicit "rename" syntax:
  let Point { x: x, y: y } = p;

-----------------
Ignoring Fields
-----------------

- Unused fields may be ignored

- :rust:`..` matches remaining fields

- Useful when only part of a struct matters

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
