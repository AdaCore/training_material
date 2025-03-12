===============================
Exercise: Generic :rust:`min`
===============================

-------------------------------
Generic :rust:`min` Program
-------------------------------

In this short exercise, you will implement a generic :rust:`min` function
that determines the minimum of two values, using the
:url:`Ord <https://doc.rust-lang.org/stable/std/cmp/trait.Ord.html>`
trait.

.. code:: rust

   use std::cmp::Ordering;

   // TODO: implement the `min` function used in `main`.

.. container:: source_include 100_generics/src/100_generics.rs :start-after://ANCHOR-main :code:rust

-------------------------------
Generic :rust:`min` Solution
-------------------------------

.. container:: source_include 100_generics/src/100_generics.rs :start-after://ANCHOR-solution :end-before://ANCHOR-main :code:rust

------------------
More Information
------------------

:url:`Ord trait <https://doc.rust-lang.org/stable/std/cmp/trait.Ord.html>`

:url:`Ordering enum <https://doc.rust-lang.org/stable/std/cmp/enum.Ordering.html>`
