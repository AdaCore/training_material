=========
Summary
=========

------------------
Pointer Programs
------------------

* Pointers are supported in SPARK

  - All kinds of pointers are supported
  - Access-to-constant is all the way down
  - General access cannot be deallocated

* Ownership policy is key

  - Ensures absence of interferences
  - Constrains code and data structures

    + No cyclic data structures

* Loops require special reasoning

  - So-called promises peek at value after borrow
  - Useful in loop invariants
