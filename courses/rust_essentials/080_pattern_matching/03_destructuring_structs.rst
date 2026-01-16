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

.. code:: rust

   struct Point {
      x: i32,
      y: i32,
   }

   let p = Point { x: 3, y: 4 };

   // Explicitly binding fields to different variable names
   // Avoids shorthand and shows custom naming
   let Point { x: horizontal, y: vertical } = p;

   println!("Horizontal: {}, Vertical: {}", horizontal, vertical);

-------------------
Shorthand Binding
-------------------

- Field names can be reused as bindings

- Reduces repetition for common cases

.. code:: rust

   let Point { x, y } = p;

   // equivalent to:
   // let Point { x: x, y: y } = p;

-----------------
Ignoring Fields
-----------------

- Unused fields may be ignored

- :rust:`..` matches remaining fields

- Useful when only part of a struct matters

.. code:: rust

   let Point { x, .. } = p;

--------------------------
Patterns Are Declarative
--------------------------

- Patterns describe *what* shape to match

- Matching is not procedural field access

- This style scales well as structures grow
