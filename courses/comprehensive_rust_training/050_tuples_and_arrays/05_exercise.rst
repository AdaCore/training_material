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

Use an array such as the above to write a function :rust:`transpose` which
will transpose a matrix (turn rows into columns):

Transpose

.. math::

   \begin{bmatrix}
      1 & 2 & 3 \\
      4 & 5 & 6 \\
      7 & 8 & 9
   \end{bmatrix}

into

.. math::

   \begin{bmatrix}
      1 & 4 & 7 \\
      2 & 5 & 8 \\
      3 & 6 & 9
   \end{bmatrix}

Copy the code below to https://play.rust-lang.org/ and implement the
function. This function only operates on 3x3 matrices.

::

   // TODO: remove this when you're done with your implementation.
   #![allow(unused_variables, dead_code)]

   {{#include exercise.rs:transpose}}
       unimplemented!()
   }

   {{#include exercise.rs:tests}}

   {{#include exercise.rs:main}}
