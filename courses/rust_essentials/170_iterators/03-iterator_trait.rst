================
Iterator Trait
================

----------------------------
What Is an Iterator Trait?
----------------------------

* **Definition**

  * Defines standard interface for producing sequence of values one at a time

* **Core Idea**

  * Yields elements lazily from some underlying data source

    * E.g., collection, range, computation

------------------------------
Describing an Iterator Trait
------------------------------

.. code:: rust

   trait Iterator {
       type Item;
       fn next(&mut self) -> Option<Self::Item>;
   }

* :rust:`type Item;`

  * Type of value the iterator yields

* :rust:`next`

  * Called repeatedly to get "next" value
  * Only required method
  * Returns :rust:`Option`

    * :rust:`Some(value)` - next element
    * :rust:`None` - iteration complete

----------------------------------------
Common Use Case - Iterating Over Slice
----------------------------------------

.. code:: rust

  for elem in [2, 4, 8, 16, 32] {
        println!("{}", elem);
    }

* "for loop" uses an iterator behind the scenes

  * :rust:`for` is syntactic sugar over :rust:`IntoIterator` trait
