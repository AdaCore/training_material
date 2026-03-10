======================
Defining an Iterator
======================

----------------------
What is an Iterator?
----------------------

* We've already seen simple iterators

  .. code:: rust

    let array = [2, 4, 6, 8];
    for idx in 0..3 {
       println!("{}", array[idx]);

* In defining an iterator, we need

.. container:: latex_environment scriptsize

  .. list-table::

    * - **Iteration State**
      - Current position in loop
      - :rust:`idx`

    * - **Termination Condition**
      - When do we exit the loop
      - :rust:`idx < 3`

    * - **State Update**
      - Moving to next item in loop
      - :rust:`idx = idx + 1`

    * - **Data Retrieval**
      - Look at what is at the current position
      - :rust:`array[idx]`

--------------------
Why Use Iterators?
--------------------

* Iterators bundle state, logic, and data retrieval into a single object

* Benefits include

  * Abstraction

    * Don't need to know underlying structure

  * Eliminates "off-by-one" errors and index-out-of-bounds panics

  * Often implemented as pointers

    * Internally more efficient to use pointer arithmetic

* Simple iterator example

  .. code:: rust

    for elem in [2, 4, 8, 16, 32] {
          println!("{}", elem);
      }

.. note::

  Iterators mean you don't need to worry about "how" to loop
