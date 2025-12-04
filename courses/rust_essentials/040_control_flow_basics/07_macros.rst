========
Macros
========

--------
Macros
--------

- Macros are expanded into code during compilation
- Can take a variable number of arguments
- Distinguished by a :rust:`!` at the end
- You can write your own macros

---------------
Useful Macros
---------------

- An assortment of useful macros is included in the standard library

.. code:: rust

   fn factorial(n: u32) -> u32 {
       let mut product = 1;
       for i in 1..=n {
           product *= dbg!(i);
       }
       product
   }

   fn fizzbuzz(n: u32) -> u32 {
       todo!()
   }

-  :rust:`println!(format, ..)` prints a line to standard output
-  :rust:`dbg!(expression)` logs the value of the expression and returns it
-  :rust:`todo!()` marks a bit of code as not-yet-implemented
-  :rust:`unreachable!()` marks a bit of code as unreachable

.. container:: speakernote

   The takeaway from this section is that these common conveniences exist,
   and how to use them. Why they are defined as macros, and what they
   expand to, is not especially critical.

   The course does not cover defining macros, but a later section will
   describe use of derive macros.
