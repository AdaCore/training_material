=========================
Exercise: Nested Arrays
=========================

-------------------------
Exercise: Nested Arrays
-------------------------

Arrays can contain other arrays:

.. code:: rust

   let array = [[1, 2, 3], [4, 5, 6], [7, 8, 9]];

What is the type of this variable?

Use an array such as the above to write a function ``transpose`` which
will transpose a matrix (turn rows into columns):

.. raw:: html

   <!-- mdbook-xgettext: skip -->

.. code:: bob

              TBDTBD1 2 3TBDTBD      TBD1 4 7TBD
   "transpose"TBDTBD4 5 6TBDTBD  "=="TBD2 5 8TBD
              TBDTBD7 8 9TBDTBD      TBD3 6 9TBD

Copy the code below to https://play.rust-lang.org/ and implement the
function. This function only operates on 3x3 matrices.

.. code:: rust,should_panic

   // TODO: remove this when you're done with your implementation.
   #![allow(unused_variables, dead_code)]

   {{#include exercise.rs:transpose}}
       unimplemented!()
   }

   {{#include exercise.rs:tests}}

   {{#include exercise.rs:main}}
