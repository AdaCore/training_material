========
Tuples
========

--------
Tuples
--------

.. code:: rust

   fn main() {
       let t: (i8, bool) = (7, true);
       println!("t.0: {}", t.0);
       println!("t.1: {}", t.1);
   }

---------
Details
---------

-  Like arrays, tuples have a fixed length.

-  Tuples group together values of different types into a compound type.

-  Fields of a tuple can be accessed by the period and the index of the
   value, e.g. :rust:`t.0`, :rust:`t.1`.

-  The empty tuple :rust:`()` is referred to as the :dfn:`unit type` and
   signifies absence of a return value, akin to :rust:`void` in other
   languages.
