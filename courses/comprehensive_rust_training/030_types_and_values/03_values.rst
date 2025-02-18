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

-  ``iN``, ``uN``, and ``fN`` are *N* bits wide,
-  ``isize`` and ``usize`` are the width of a pointer,
-  ``char`` is 32 bits wide,
-  ``bool`` is 8 bits wide.

.. raw:: html

---------
Details
---------

There are a few syntaxes which are not shown above:

-  All underscores in numbers can be left out, they are for legibility
   only. So ``1_000`` can be written as ``1000`` (or ``10_00``), and
   ``123_i64`` can be written as ``123i64``.

.. raw:: html

