=========
Summary
=========

-----------------
What We Covered
-----------------

* **Lifetimes**

  * How long references are valid

  * References cannot outlive data they refer to

  * Borrow checker verifies this at compile time

* **Annotations**

  * Describe relationships between references

  * Part of reference types  

  * Most are inferred automatically

* **Elision**

  * Lifetimes are still present even when not written  

  * Compiler applies fixed rules (not guesswork)

  * Explicit lifetimes needed when relationships are ambiguous

* **Structs and Enums**

  * Storing references requires lifetimes

  * Lifetimes follow same rules as functions

  * Owning data is often simpler than borrowing
