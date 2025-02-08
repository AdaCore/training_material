=================
Array Iteration
=================

-----------------
Array Iteration
-----------------

The :rust:`for` statement supports iterating over arrays (but not tuples).

.. code:: rust

   fn main() {
       let primes = [2, 3, 5, 7, 11, 13, 17, 19];
       for prime in primes {
           for i in 2..prime {
               assert_ne!(prime % i, 0);
           }
       }
   }

---------
Details
---------

This functionality uses the :rust:`IntoIterator` trait, but we haven't
covered that yet.

The :rust:`assert_ne!` macro is new here. There are also :rust:`assert_eq!` and
:rust:`assert!` macros. These are always checked, while debug-only variants
like :rust:`debug_assert!` compile to nothing in release builds.
