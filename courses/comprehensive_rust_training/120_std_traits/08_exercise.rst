=================
Exercise: ROT13
=================

-----------------
Exercise: ROT13
-----------------

In this example, you will implement the classic
`"ROT13" cipher <https://en.wikipedia.org/wiki/ROT13>`__. Copy this code to the
playground, and implement the missing bits. Only rotate ASCII alphabetic
characters, to ensure the result is still valid UTF-8.

::

   {{#include exercise.rs:head }}

   // Implement the `Read` trait for `RotDecoder`.

   {{#include exercise.rs:main }}

What happens if you chain two :rust:`RotDecoder` instances together, each
rotating by 13 characters?
