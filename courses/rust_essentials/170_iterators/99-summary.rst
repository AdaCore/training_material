=========
Summary
=========

-----------------
What We Covered
-----------------

* **Defining an Iterator**

  * Mechanism used to span iterable content (like arrays)
  * All languages have some iteration capability

    * :rust:`for` loop is most obvious

* **Iterator as a Trait**

  * Trait-based mechanism to traverse any iterable type
  * Only needs an associated type and a :rust:`next` method

* **Additional Iterator Trait Methods**

  * Lots of other methods available
  * *Adapter* methods generate a new iterator with different behavior

    * e.g., :rust:`map`, :rust:`filter`

  * *Consumer* methods move the iterator content

    * e.g., :rust:`sum`, :rust:`count`

* **Collecting Data via Iterator**

  * Transform iterator into a specific collection (e.g. :rust:`Vec`)

    * Iterates through every element and packs them into the target type

* **Converting to an Iterator**

  * Use :rust:`IntoIterator` to define converting a type to an iterator
  * Allows collection to be used directly in :rust:`for` loop
