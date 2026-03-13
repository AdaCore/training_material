================
Helper Methods
================

------------------------------
Extending the Iterator Trait
------------------------------

* We know :rust:`next` is the only helper that *must* be implemented

  * But there are lots more that *can* be implemented

* Two main categories 

  * **Iterator Adapters**

    * Transform iterator into a new iterator (e.g., :rust:`map`, :rust:`filter`)

  * **Consumers**

    * Pull values out of iterator to produce result (e.g., :rust:`sum`, :rust:`collect`)

--------------------------------
Getting an Iterator ("iter()")
--------------------------------

* Collections are not iterators by default

  * Need to first create an iterator

  .. code:: rust
    :number-lines: 3

    let numbers = vec![10, 20, 30];
    for n in numbers.iter() {
        println!("{n}");
    }

  * Output

    :command:`10`

    :command:`20`

    :command:`30`

* :rust:`iter()` (line 4) creates the collection iterator

  * Returns an iterator over references
  * Does **not** consume the collection
  * Allows use of iterator adapters

--------------------------
Common Iterator Adapters
--------------------------

* :rust:`map` **- transform values during iteration**

  .. container:: latex_environment tiny

    .. code:: rust

      let numbers = vec![10, 20, 30];
      for elem in numbers.iter().map(|n| n * 2) {
          println!("{elem}");
      }

    * **Output**

      :command:`20`

      :command:`40`

      :command:`60`

* :rust:`filter` **- select values matching condition**

  .. container:: latex_environment tiny

    .. code:: rust

      let numbers = vec!11, 12, 13, 14];
      for elem in numbers.iter().filter(|n| *n % 2 == 0) {
          println!("{elem}");
      }

    * **Output**

      :command:`12`

      :command:`14`

.. note::

  Adapters are used to look at each element in the collection

------------------
Common Consumers
------------------

* :rust:`sum` **- add all values and return a single value**

  .. container:: latex_environment tiny

    .. code:: rust

      let numbers = [1, 2, 3, 4, 5];
      // .iter() creates the stream of references
      // .sum() pulls them all off and adds them up
      let total: i32 = numbers.iter().sum();
      println!("The total is: {total}");

    * **Output**

      :command:`15`

* :rust:`any` **- return True if any value matches condition**

  .. container:: latex_environment tiny

    .. code:: rust

      let temperatures = [22, 28, -2, 15, 30];

      // .iter() creates the stream of references
      // .any() looks for the first item that satisfies the closure
      if temperatures.iter().any(|&t| t < 0) {
          println!("Warning: Freezing temperatures detected!");
      } else {
          println!("All temperatures are above freezing.");
      }

    * **Output**

      :command:`Warning: Freezing temperatures detected!`

    * :rust:`all` returns True if **all** values match

.. note::

  Consumers are used to look at the collection as a whole

-----------------------------
Declarative Data Processing
-----------------------------

* Can use "chaining" instead of loops and conditionals

  * Easier to read

.. code:: rust
  :font-size: scriptsize

  fn main() {
      let result: i32 = (1..=10)        // Range: 1, 2, 3, ..., 10
          .filter(|x| x % 2 == 0)       // Keep only even: 2, 4, 6, 8, 10
          .map(|x| x * x)               // Square them: 4, 16, 36, 64, 100
          .sum();                       // Add them up: 220

      println!("Sum of even squares: {}", result);
  }

:command:`Sum of even squares: 220`

----------------------------
Reliability and Maintenace
----------------------------

.. container:: latex_environment small

  * Lazy evaluation

    * Adapters do nothing until a "Consumer" (like :rust:`sum` or :rust:`count`) is called

      * You can build a complex pipeline and it won't execute until the very end.

  * Intent over implementation

    * Intent: "Create variable, loop, check condition, square it, add to variable"
    * Implement: "Filter evens, map to squares, sum"
