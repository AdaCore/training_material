======================
Defining an Iterator
======================

----------------------
What Is an Iterator?
----------------------

* Simplest form of *iteration* is a "for loop"

  .. code:: rust

    let array = [2, 4, 6, 8];
    for idx in 0..4 {
       println!("{}", array[idx]);

* Iterations contain the following information

.. container:: latex_environment scriptsize

  .. list-table::
    :stub-columns: 1

    * - *Iteration State*
      - Current position in loop
      - :rust:`idx`

    * - *Termination Condition*
      - When do we exit the loop
      - :rust:`idx < 3`

    * - *State Update*
      - Moving to next item in loop
      - :rust:`idx = idx + 1`

    * - *Data Retrieval*
      - Look at what is at the current position
      - :rust:`array[idx]`

---------------------
What Is an Iterator
---------------------

* Provides standard way to access elements of a collection

  * One at a time

    * Typically in sequence

  * Without exposing collection’s internal structure
  * Using types, traits, and methods

* Benefits include

  * Reduces "off-by-one" errors and index-out-of-bounds panics
  * Lightweight structs that hold iteration state

    * Compile to very efficient loops

* Simple iterator example

  .. code:: rust

    let numbers = vec![1, 2, 3];

    // .iter() creates the iterator
    let it = numbers.iter()
        for num in it {
            println!("{}", num);
        }

.. note::

  Iterators mean you don't need to worry about "how" to loop
