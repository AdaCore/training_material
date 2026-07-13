==============
Introduction
==============

----------------
Topics Covered
----------------

- **Generic Type Parameter**

  - Definition and instantiation

  - Define multiple generic type

- **Constraints and Properties**

  - Traits add functionalities

    - And restrictions

- **Generic Traits and Constants**

   - Define generic constructs over traits and numbers


-------------------------
The Notion of a Pattern
-------------------------

* Sometimes algorithms can be abstracted from types and subprograms

.. code:: Rust

  fn swap_int(left: i32, right: i32) -> (i32, i32) {
    (right, left)
  }

  fn swap_float(left: f64, right: f64) -> (f64, f64) {
    (right, left)
  }

*  A common pattern could be extracted, with only some parts to replace

.. code:: Rust

  fn swap<T>(left: T, right: T) -> (T, T) {
      (right, left)
  }
