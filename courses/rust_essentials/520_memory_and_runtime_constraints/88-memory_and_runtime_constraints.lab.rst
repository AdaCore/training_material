=====
Lab
=====

--------------
Introduction
--------------

This lab will demonstrate your understanding of different types of memory budgets

  * Fixed capacity storage
  * Dynamically allocated storage
  * Caller-supplied storage

The example is a simplistic averaging routine - total the number of some
supplied values and determine the average of all the values

The main program will test that each module behaves correctly

.. tip::

  There will be hints along the way so you don't get stuck forever!

----------
Overview
----------

* The lab is a complete Rust crate, but it will not build yet

* Your goal is to fully implement the structures and the test function

    * :rust:`Fixed` defines a fixed length holder for the maximum number of values
    * :rust:`Dynamic` uses a growable :rust:`Vec`, but still limits the number of samples to :rust:`MAX_SAMPLES`
    * :rust:`Caller` accepts a holder for storing values

.. note::

  :rust:`Caller` is trickier, so the structure and :rust:`new` function are already defined

--------------
Instructions
--------------

* Implement a :rust:`sample_data` module containing data for

  * Maximum number of values (:rust:`MAX_SAMPLES`)
  * List of values (:rust:`VALUES`)
  * Expected average (:rust:`AVERAGE`)

* For each memory model (:rust:`fixed`, :rust:`dynamic`, :rust:`caller` modules)

  * Implement content of structure
  * Implement :rust:`new` associated function
  * Implement :rust:`Logger` trait

* Complete implementation of the :rust:`common_test` function

  * Once implemented for one module, it should work for all modules

-------
Hints
-------

* Easiest to start with :rust:`sample_data`

  * You will need those values for the other modules

* :rust:`Fixed` is probably the simplest memory module

  * Just use a simple array

* Comments in the prompt files should help

.. tip:

  If you get stuck, refer to the :filename:`answer` folder for hints

----------
Solution
----------

* Output from running full test should look something like

.. code:: output

  Testing a fixed buffer
  Exceeded capacity
  Average 6 over 9 values
  Final capacity 9

  Testing a caller-supplied buffer
  Exceeded capacity
  Average 6 over 9 values
  Final capacity 9

  Testing a dynamic buffer
  Exceeded capacity
  Average 6 over 9 values
  Final capacity 16
