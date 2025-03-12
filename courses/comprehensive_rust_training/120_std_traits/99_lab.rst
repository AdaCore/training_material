=================
Exercise: ROT13
=================

-----------------
ROT13 Problem
-----------------

In this example, you will implement the classic
:url:`"ROT13" cipher <https://en.wikipedia.org/wiki/ROT13>`. Copy this code to the
playground, and implement the missing bits. Only rotate ASCII alphabetic
characters, to ensure the result is still valid UTF-8.

.. container:: source_include 120_std_traits/src/120_std_traits.rs :start-after://ANCHOR-head :end-before://ANCHOR-solution :code:rust :number-lines:1

   // Implement the `Read` trait for `RotDecoder`.
-----------------
ROT13 Main
-----------------

.. container:: source_include 120_std_traits/src/120_std_traits.rs :start-after://ANCHOR-main :code:rust :number-lines:1

-----------------
ROT13 Solution
-----------------

.. container:: source_include 120_std_traits/src/120_std_traits.rs :start-after://ANCHOR-solution :end-before://ANCHOR-main :code:rust :number-lines:1

What happens if you chain two :rust:`RotDecoder` instances together, each
rotating by 13 characters?
