=========
Summary
=========

-----------------
What We Covered
-----------------

* **Lifetimes**

  * Lifetime describes how long reference is valid

  * References cannot outlive data they refer to

  * Borrow checker verifies this at compile time

* **Annotations**

  * Lifetime annotations describe relationships between references

  * Lifetimes are part of reference types  

  * Most annotations are inferred automatically

* **Elision**

  * Lifetimes are still present even when not written  

  * Compiler applies fixed rules (not guesswork)

  * Explicit lifetimes needed when relationships are ambiguous

* **Data Structures**

  * Structs storing references require lifetimes

  * Lifetimes in structs follow same rules as functions

  * Owning data is often simpler than borrowing
