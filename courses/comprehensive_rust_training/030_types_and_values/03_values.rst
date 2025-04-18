========
Values
========

--------
Values
--------

Here are some basic built-in types, and the syntax for literal values of
each type.

.. list-table::
   :header-rows: 1

   * -
     - Types
     - Literals

   * - Signed integers
     - :rust:`i8`, :rust:`i16`, :rust:`i32`, :rust:`i64`, :rust:`i128`, :rust:`isize`
     - :rust:`-10`, :rust:`0`, :rust:`1_000`, :rust:`123_i64`

   * - Unsigned integers
     - :rust:`u8`, :rust:`u16`, :rust:`u32`, :rust:`u64`, :rust:`u128`, :rust:`usize`
     - :rust:`0`, :rust:`123`, :rust:`10_u16`

   * - Floating point numbers
     - :rust:`f32`, :rust:`f64`
     - :rust:`3.14`, :rust:`-10.0e20`, :rust:`2_f32`

   * - Unicode scalar values
     - :rust:`char`
     - :rust:`'a'`, ':math:`\alpha`', ':math:`\infty`'

   * - Booleans
     - :rust:`bool`
     - :rust:`true`, :rust:`false`

The types have widths as follows:

-  :rust:`iN`, :rust:`uN`, and :rust:`fN` are *N* bits wide,
-  :rust:`isize` and :rust:`usize` are the width of a pointer,
-  :rust:`char` is 32 bits wide,
-  :rust:`bool` is 8 bits wide.

------------------------
Readability of Numbers
------------------------

- Underscores in numbers can be left out - they are for legibility only

  - :rust:`1_000` can be written as :rust:`1000` (or :rust:`10_00`)
  - :rust:`123_i64` can be written as :rust:`123i64`.
