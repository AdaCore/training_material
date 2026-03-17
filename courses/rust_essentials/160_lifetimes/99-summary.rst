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

  * Lifetimes are part of reference types  

  * Most annotations are inferred automatically

* **Elision**

  * Lifetimes are still present even when not written  

  * The compiler applies fixed rules (not guesswork)

  * Explicit lifetimes are needed when relationships are ambiguous

* **Data Structures**

  * Structs storing references require lifetimes

  * Lifetimes in structs follow the same rules as functions

  * Owning data is often simpler than borrowing
