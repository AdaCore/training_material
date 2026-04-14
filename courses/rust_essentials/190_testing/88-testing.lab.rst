==========================
Exercise: Luhn Algorithm
==========================

--------------------------
Luhn Algorithm Setup
--------------------------

The :url:`Luhn algorithm <https://en.wikipedia.org/wiki/Luhn_algorithm>` is
used to validate credit card numbers. The algorithm takes a string as
input and does the following to validate the credit card number:

-  Ignore all spaces. Reject numbers with fewer than two digits.

-  Moving from **right to left**, double every second digit: for the
   number :rust:`1234`, we double :rust:`3` and :rust:`1`. For the number :rust:`98765`,
   we double :rust:`6` and :rust:`8`.

-  After doubling a digit, sum the digits if the result is greater than
   9. So doubling :rust:`7` becomes :rust:`14` which becomes :rust:`1 + 4 = 5`.

-  Sum all the undoubled and doubled digits.

-  The credit card number is valid if the sum ends with :rust:`0`.

The provided code provides a buggy implementation of the luhn algorithm,
along with two basic unit tests that confirm that most of the algorithm
is implemented correctly.

--------------------------
Luhn Algorithm Problem
--------------------------

Copy the code below to https://play.rust-lang.org/ and write additional
tests to uncover bugs in the provided implementation, fixing any bugs
you find.

.. container:: source_include 190_testing/src/190_testing.rs :start-after://ANCHOR-buggy :end-before://ANCHOR-solution :code:rust

--------------------------------
Luhn Algorithm - Initial Tests
--------------------------------

.. container:: source_include 190_testing/src/190_testing.rs :start-after://ANCHOR-some_tests :end-before://ANCHOR-more_tests :code:rust

.. container:: source_include 190_testing/src/190_testing.rs :start-after://ANCHOR-end_tests :code:rust

--------------------------------
Luhn Algorithm Solution - Code
--------------------------------

.. container:: source_include 190_testing/src/190_testing.rs :start-after://ANCHOR-solution :end-before://ANCHOR-main :code:rust

---------------------------------
Luhn Algorithm Solution - Tests
---------------------------------

.. container:: source_include 190_testing/src/190_testing.rs :start-after://ANCHOR-some_tests :end-before://ANCHOR-more_tests :code:rust

.. container:: source_include 190_testing/src/190_testing.rs :start-after://ANCHOR-more_tests :end-before://ANCHOR-end_tests :code:rust

.. container:: source_include 190_testing/src/190_testing.rs :start-after://ANCHOR-end_tests :code:rust

