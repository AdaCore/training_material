=========
Summary
=========

-----------------
What We Covered
-----------------

* **Panics**

  * Unrecoverable errors

    * Impossible states where program should not continue

* **Result** 

  * Enum-based way to handle recoverable errors
  * Requires handling of success and failure

* **The** :rust:`?` **Operator**

  * Syntax shortcut that keeps code clean
  * Automatically unwraps success values

    * Or returns error early to caller

* **The** :rust:`Error` **Trait**

  * Interface to allow conversion of errors
  * Standard way to show messages

    * Includes tracing back to the root cause

* :rust:`thiserror` **vs.** :rust:`anyhow`

  * :rust:`thiserror`

    * Used in libraries to create matchable error types

  * :rust:`anyhow`

    * Used in applications to create human-readable errors
