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
| Signed        | ``i8``, ``i16``, ``i32``,     | ``-10``, ``0``,     |
| integers      | ``i64``, ``i128``, ``isize``  | ``1_000``,          |
|               |                               | ``123_i64``         |
+---------------+-------------------------------+---------------------+
| Unsigned      | ``u8``, ``u16``, ``u32``,     | ``0``, ``123``,     |
| integers      | ``u64``, ``u128``, ``usize``  | ``10_u16``          |
+---------------+-------------------------------+---------------------+
| Floating      | ``f32``, ``f64``              | ``3.14``,           |
| point numbers |                               | ``-10.0e20``,       |
|               |                               | ``2_f32``           |
+---------------+-------------------------------+---------------------+
| Unicode       | ``char``                      | ``'a'``,            |
| scalar values |                               | ':math:`\alpha`',   |
|               |                               | ':math:`\infty`'    |
+---------------+-------------------------------+---------------------+
| Booleans      | ``bool``                      | ``true``, ``false`` |
+---------------+-------------------------------+---------------------+

The types have widths as follows:

-  :rust:`iN`, :rust:`uN`, and :rust:`fN` are *N* bits wide,
-  :rust:`isize` and :rust:`usize` are the width of a pointer,
-  :rust:`char` is 32 bits wide,
-  :rust:`bool` is 8 bits wide.

---------
Details
---------

There are a few syntaxes which are not shown above:

-  All underscores in numbers can be left out, they are for legibility
   only. So :rust:`1_000` can be written as :rust:`1000` (or :rust:`10_00`), and
   :rust:`123_i64` can be written as :rust:`123i64`.
