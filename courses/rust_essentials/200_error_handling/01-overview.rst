==========
Overview
==========

-----------------------
Rust Error Philosophy
-----------------------

* Errors are data, not drama!

  * Failures are explicit values

    * Not hidden control flow

* Errors are visible in function signatures
* Compiler ensures they are handled

  * No access to invalid data

* Failures do not show up mysteriously

  * Visible and enforced

.. list-table::
  :header-rows: 1

  * - **Language Style**
    - **Error Visibility**

  * - Exceptions
    - Hidden control flow

  * - Error codes
    - Easy to ignore

  * - Error wrappers (e.g., :rust:`Result`)
    - Explicit and enforced

--------------------------------------
Expected Errors vs. Logic Violations
--------------------------------------

* Recoverable errors should be handled by the code

  * File not found
  * Network timeout
  * Invalid user input

* Unrecoverable errors mean we need to reboot the system

  * Impossible state reached
  * Violated assumptions/invariant
  * Logic error or other bug

.. note::

  Rust separates bugs from expected failures
