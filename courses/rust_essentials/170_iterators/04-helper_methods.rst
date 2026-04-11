================
Helper Methods
================

------------------------------
Extending the Iterator Trait
------------------------------

* :rust:`next` is the only *required* method

  * *Many* more *can* be implemented

* Iterator Adapters

  * Transform iterator into a **new** iterator

    * E.g., :rust:`map`, :rust:`filter`

  * Useful in chaining

    * Consecutive iteration

* Consumers

  * Drive the iterator to produce final value

    * E.g., :rust:`sum`, :rust:`collect`

  * Ends any chaining

    * Result is **not** an iterator

--------------------------------
Getting an Iterator - "iter()"
--------------------------------

* Collections itself us not an iterator

  * Call :rust:`.iter()` to create iterator

  .. code:: rust
    :number-lines: 3

    let numbers = vec![10, 20, 30];
    for n in numbers.iter() {
        println!("{n}");
    }

  :command:`10`

  :command:`20`

  :command:`30`

* :rust:`iter()` (line 4) creates the collection iterator

  * Returns an iterator over references (:rust:`&T`)
  * Does **not** consume the collection
  * Allows use of iterator adapters

--------------------------
Common Iterator Adapters
--------------------------

* :rust:`map` **- transform values during iteration**

  .. code:: rust

    let numbers = vec![10, 20, 30];
    for elem in numbers.iter().map(|n| n * 2) {
        println!("{elem}");
    }

  :command:`20`

  :command:`40`

  :command:`60`

* :rust:`filter` **- select values matching condition**

  .. code:: rust

    let numbers = vec!11, 12, 13, 14];
    for elem in numbers.iter().filter(|n| *n % 2 == 0) {
        println!("{elem}");
    }

  :command:`12`

  :command:`14`

.. note::

  Adapters return a **new** iterator - they don't consume values

------------------
Common Consumers
------------------

* :rust:`sum` **- add all values and return a single value**

    .. code:: rust

      let numbers = [1, 2, 3, 4, 5];
      // '.iter()' creates the stream of references
      // '.sum()' pulls them all off and adds them up
      let total: i32 = numbers.iter().sum();
      println!("The total is: {total}");

    :command:`15`

* :rust:`any` **- return True if any value matches condition**

    .. code:: rust

      let temperatures = [22, 28, -2, 15, 30];

      // .iter() creates the stream of references
      // .any() looks for the first item that satisfies the closure
      if temperatures.iter().any(|&t| t < 0) {
          println!("Warning: Freezing temperatures detected!");
      } else {
          println!("All temperatures are above freezing.");
      }

    :command:`Warning: Freezing temperatures detected!`

    :rust:`all` *returns True if* **all** *values match*

.. note::

  Consumers return a single result from the iteration

-----------------------------
Declarative Data Processing
-----------------------------

**Can use "chaining" instead of loops and conditionals**

  * Easier to read

.. code:: rust
  :font-size: scriptsize

  fn main() {
      let result: i32 = (1..=10)        // Range: 1, 2, 3, ..., 10
          .filter(|x| x % 2 == 0)       // Keep even: 2, 4, 6, 8, 10
          .map(|x| x * x)               // Square: 4, 16, 36, 64, 100
          .sum();                       // Total: 220

      println!("Sum of even squares: {}", result);
  }

:command:`Sum of even squares: 220`

.. note::

  Chaining allows you to create a new set of data before consuming

    Modify values, skip values, etc.

----------------------------
Reliability and Maintenace
----------------------------

* Lazy evaluation

  * Adapters do nothing until a "Consumer" is called

    * :rust:`sum` or :rust:`count`, etc.

  * Pipeline runs only when needed

* Imperative vs iterator implementation

  * :rust:`for` loop

    .. code:: rust

      let mut sum = 0;
      for x in 1..=10 {
          if x % 2 == 0 {
              sum += x * x;
          }
      }

  * Chaining and collection

    .. code:: rust

      let sum: i32 = (1..=10)
          .filter(|x| x % 2 == 0) // Keep only even numbers
          .map(|x| x * x)         // Square them
          .sum();                 // Add them all up
