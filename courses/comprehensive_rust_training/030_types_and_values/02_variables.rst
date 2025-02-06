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

.. raw:: html

---------
Details
---------

-  Uncomment the :rust:`x = 20` to demonstrate that variables are immutable
   by default. Add the :rust:`mut` keyword to allow changes.

-  Warnings are enabled for this slide, such as for unused variables or
   unnecessary :rust:`mut`. These are omitted in most slides to avoid
   distracting warnings. Try removing the mutation but leaving the
   :rust:`mut` keyword in place.

-  The :rust:`i32` here is the type of the variable. This must be known at
   compile time, but type inference (covered later) allows the
   programmer to omit it in many cases.

.. raw:: html

