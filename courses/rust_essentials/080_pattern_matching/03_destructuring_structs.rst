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

   let Point { x, .. } = p;

--------------------------
Patterns Are Declarative
--------------------------

- Patterns describe *what* shape to match

- Matching is not procedural field access

- This style scales well as structures grow
