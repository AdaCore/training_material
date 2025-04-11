======================
Motivating Iterators
======================

----------------------
Motivating Iterators
----------------------

If you want to iterate over the contents of an array, you'll need to
define:

-  Some state to keep track of where you are in the iteration process,
   e.g. an index.
-  A condition to determine when iteration is done.
-  Logic for updating the state of iteration each loop.
-  Logic for fetching each element using that iteration state.

In a C-style for loop you declare these things directly:

.. code:: c

   for (int i = 0; i < array_len; i += 1) {
       int elem = array[i];
   }

In Rust we bundle this state and logic together into an object known as
an :dfn:`iterator`.

------------------------------------
Rust's Implementation of Iterators
------------------------------------

-  A C-style :cpp:`for` loop shows how iteration requires some state and some logic

-  Rust doesn't have a C-style :cpp:`for` loop, but we can express the same
   thing with :rust:`while`:

   .. code:: rust

      let array = [2, 4, 6, 8];
      let mut i = 0;
      while i < array.len() {
          let elem = array[i];
          i += 1;
      }

-----------------
More to Explore
-----------------

There's another way to express array iteration using :cpp:`for` in C and
C++: You can use a pointer to the front and a pointer to the end of the
array and then compare those pointers to determine when the loop should
end.

.. code:: c

   for (int *ptr = array; ptr < array + len; ptr += 1) {
       int elem = *ptr;
   }

This is how Rust's slice and array iterators work under the hood
(though implemented as a Rust iterator).
