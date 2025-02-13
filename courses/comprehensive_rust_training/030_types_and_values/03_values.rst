========
Values
========

--------
Values
--------

Here are some basic built-in types, and the syntax for literal values of
each type.

+---------------+-------------------------------+---------------------+
|               | Types                         | Literals            |
+===============+===============================+=====================+
| Signed        | :rust:`i8`, :rust:`i16`, :rust:`i32`,     | :rust:`-10`, :rust:`0`,     |
| integers      | :rust:`i64`, :rust:`i128`, :rust:`isize`  | :rust:`1_000`,          |
|               |                               | :rust:`123_i64`         |
+---------------+-------------------------------+---------------------+
| Unsigned      | :rust:`u8`, :rust:`u16`, :rust:`u32`,     | :rust:`0`, :rust:`123`,     |
| integers      | :rust:`u64`, :rust:`u128`, :rust:`usize`  | :rust:`10_u16`          |
+---------------+-------------------------------+---------------------+
| Floating      | :rust:`f32`, :rust:`f64`              | :rust:`3.14`,           |
| point numbers |                               | :rust:`-10.0e20`,       |
|               |                               | :rust:`2_f32`           |
+---------------+-------------------------------+---------------------+
| Unicode       | :rust:`char`                      | :rust:`'a'`,            |
| scalar values |                               | ':math:`\alpha`',   |
|               |                               | ':math:`\infty`'    |
+---------------+-------------------------------+---------------------+
| Booleans      | :rust:`bool`                      | :rust:`true`, :rust:`false` |
+---------------+-------------------------------+---------------------+

The types have widths as follows:

-  :rust:`iN`, :rust:`uN`, and :rust:`fN` are *N* bits wide,
-  :rust:`isize` and :rust:`usize` are the width of a pointer,
-  :rust:`char` is 32 bits wide,
-  :rust:`bool` is 8 bits wide.

.. raw:: html

---------
Details
---------

There are a few syntaxes which are not shown above:

-  All underscores in numbers can be left out, they are for legibility
   only. So :rust:`1_000` can be written as :rust:`1000` (or :rust:`10_00`), and
   :rust:`123_i64` can be written as :rust:`123i64`.

.. raw:: html

