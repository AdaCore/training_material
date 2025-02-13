===================
Blocks and Scopes
===================

-------------------
Blocks and Scopes
-------------------

--------
Blocks
--------

A block in Rust contains a sequence of expressions, enclosed by braces
:rust:`{}`. Each block has a value and a type, which are those of the last
expression of the block:

.. code:: rust

   fn main() {
       let z = 13;
       let x = {
           let y = 10;
           println!("y: {y}");
           z - y
       };
       println!("x: {x}");
   }

If the last expression ends with :rust:`;`, then the resulting value and
type is :rust:`()`.

.. raw:: html

---------
Details
---------

-  You can show how the value of the block changes by changing the last
   line in the block. For instance, adding/removing a semicolon or using
   a :rust:`return`.

.. raw:: html

