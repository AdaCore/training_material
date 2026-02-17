=========
Summary
=========

-----------------
What We Covered
-----------------

* **Panics**

  * "Stop the world" bugs 
  * Impossible states where the program cannot safely continue.

* **Result** 

  * Standard enum-based way to handle expected errors
  * Forces the developer to explicitly account for success or failure at compile-time

* **The** :rust:`?` **Operator**

  * Syntax shortcut that keeps code clean
  * Automatically unwraps success values or returning errors early to the caller

* **The** :rust:`Error` **Trait**

  * Common interface to allo different error types to work together
  * Provides standard way to show messages and trace back to the root cause

* :rust:`thiserror` **vs.** :rust:`anyhow`

  * :rust:`thiserror` used in libraries to create structured, matchable error types
  * :rust:`anyhow` used in applications to easily add context to any error that occurs
