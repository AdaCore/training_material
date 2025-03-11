=========================
Exercise: Nested Arrays
=========================

-------------------------
Nested Arrays Problem
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

.. code:: rust

   fn transpose(matrix: [[i32; 3]; 3]) -> [[i32; 3]; 3] {
       todo!()
   }

   fn main() {
       let matrix = [
           [101, 102, 103], //  comment makes rustfmt add newline
           [201, 202, 203],
           [301, 302, 303],
       ];

       dbg!(matrix);
       let transposed = transpose(matrix);
       dbg!(transposed);
   }

-------------------------
Nested Arrays Solution
-------------------------

.. code:: rust

   fn transpose(matrix: [[i32; 3]; 3]) -> [[i32; 3]; 3] {
       let mut result = [[0; 3]; 3];
       for i in 0..3 {
           for j in 0..3 {
               result[j][i] = matrix[i][j];
           }
       }
       result
   }

   fn main() {
       let matrix = [
           [101, 102, 103], //  comment makes rustfmt add newline
           [201, 202, 203],
           [301, 302, 303],
       ];

       dbg!(matrix);
       let transposed = transpose(matrix);
       dbg!(transposed);
   }
