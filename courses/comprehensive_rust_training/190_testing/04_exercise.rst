==========================
Exercise: Luhn Algorithm
==========================

--------------------------
Exercise: Luhn Algorithm
--------------------------

The `Luhn algorithm <https://en.wikipedia.org/wiki/Luhn_algorithm>`__ is
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

Copy the code below to https://play.rust-lang.org/ and write additional
tests to uncover bugs in the provided implementation, fixing any bugs
you find.

::

   {{#include exercise.rs:luhn}}

   {{#include exercise.rs:unit-tests}}
   }
