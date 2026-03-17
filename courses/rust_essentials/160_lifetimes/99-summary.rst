=========
Summary
=========

-----------------
What We Covered
-----------------

* **Lifetimes**

  * A lifetime describes how long a reference is valid

  * References cannot outlive the data they refer to

  * The borrow checker verifies this at compile time

* **Annotations**

  * Lifetime annotations describe relationships between references

  * Elision rules allow most lifetimes to remain implicit

  * Explicit lifetimes are needed when relationships are ambiguous
