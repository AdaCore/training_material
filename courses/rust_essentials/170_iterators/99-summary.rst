=========
Summary
=========

-----------------
What We Covered
-----------------

* **Defining an Iterator**

  * Mechanism used to span iterable content (like arrays)
  * All languages have some iteration capability

* **Iterator as a Trait**

  * Trait-based mechanism to traverse any iterable type
  * Only needs an associated type and a :rust:`next` method

* **Additional Iterator Trait Methods**

  * *Iterator Adapter* methods - e.g., :rust:`map`, :rust:`filter`
  * *Consumer* methods - e.g., :rust:`sum`, :rust:`count`

    * :rust:`collect` is just a fancy consumer

* **Converting using** :rust:`IntoIterator`

  * Allows collection to be used directly in :rust:`for` loop
