================
Helper Methods
================

------------------------------
Extending the Iterator Trait
------------------------------

* We know :rust:`next` is the only helper that *must* be implemented

  * But there are lots more that *can* be implemented

* Two categories 

  * **Iterator Adapters**

    * Transform iterator into a new iterator (e.g., :rust:`map`, :rust:`filter`)

  * **Consumers**

    * Pull values out of iterator to produce result (e.g., :rust:`sum`, :rust`count`)

--------------------------
Common Iterator Adapters
--------------------------

.. list-table::

  * - :rust:`map`
    - Apply function to each element

  * - :rust:`filter`
    - Keep only elements that satisfy predicate

  * - :rust:`iter`
    - Create iterator over references to elements

  * - :rust:`enumerate`
    - Converts iterator into *(index, value)* pair

------------------
Common Consumers
------------------

.. list-table:: 

  * - :rust:`collect`
    - Convert iterator to collection (e.g. :rust:`Vec`, :rust:`String`)

  * - :rust:`sum`
    - Adds all elements

  * - :rust:`any`
    - Return True if **any** element satisfies predicate

  * - :rust:`all`
    - Return True if **all** element satisfy predicate

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

    * Adapters do nothing until a "Consumer" (like :rust:`sum` or :rust:`collect`) is called

      * You can build a complex pipeline and it won't execute until the very end.

  * Intent over implementation

    * Intent: "Create a variable, loop, check a condition, square it, add to variable..."
    * Implement: "Filter evens, map to squares, sum"
