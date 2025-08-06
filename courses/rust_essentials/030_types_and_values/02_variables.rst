===========
Variables
===========

-----------
Variables
-----------

Rust provides type safety via static typing. Variable bindings are made
with :rust:`let`:

.. code:: rust

   fn main() {
       let x: i32 = 10;
       println!("x: {x}");
       // x = 20;
       // println!("x: {x}");
   }

-------------------
Things To Explore
-------------------

- Uncomment :rust:`x = 20` to show variables are immutable by default

  - Add the :rust:`mut` keyword to allow changes.

- Warnings are enabled for this slide, such as for unused variables or unnecessary :rust:`mut`.

  - These are omitted in most slides to avoid distracting warnings.
  - Try removing the mutation but leaving the :rust:`mut` keyword in place.

- :rust:`i32` is the type of the variable.

  - Must be known at compile time
  - Type inference (covered later) allows the programmer to omit it in many cases.
