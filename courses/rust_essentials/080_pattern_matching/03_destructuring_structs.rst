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

---------------------
Basic Destructuring
---------------------

- Fields can be bound directly to names

   - Order does not matter

- Names may differ from field names

- Field shorthand can be used when names match

.. code:: rust

   struct Point {
       x: i32,
       y: i32,
   }

   let p = Point { x: 3, y: 4 };

   // Longhand: Renaming fields to new variables
   let Point { x: horizontal, y: vertical } = p;
   // horizontal = 3, vertical = 4

   // Shorthand: Variable names match field names
   let Point { x, y } = p;
   // x = 3, y = 4

   // Order independence
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

   :rust:`..` *captures* specific nammed fields and *ignores* the rest
