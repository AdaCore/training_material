=========================
Exercise: Nested Arrays
=========================

-------------------------
Nested Arrays Setup
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

-------------------------
Nested Arrays Problem
-------------------------

Copy the code below to https://play.rust-lang.org/ and implement the
function. This function only operates on 3x3 matrices.

.. code:: rust

   fn transpose(matrix: [[i32; 3]; 3]) -> [[i32; 3]; 3] {
       todo!()
   }

.. container:: source_include 050_tuples_and_arrays/src/050_tuples_and_arrays.rs :start-after://ANCHOR-main :code:rust

-------------------------
Nested Arrays Solution
-------------------------

.. container:: source_include 050_tuples_and_arrays/src/050_tuples_and_arrays.rs :start-after://ANCHOR-solution :code:rust :end-before://ANCHOR-tests
