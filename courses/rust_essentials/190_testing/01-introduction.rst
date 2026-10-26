==============
Introduction
==============

----------------
Topics Covered
----------------

* **Building unit tests**

  - Creating a test framework
  - Specifying and running tests
  - Verifying test results

* **Improving unit tests**

  - Adding information to assertion failures
  - Capturing expected panics
  - Using :rust:`Result` to connect test conditions

* **Running unit tests**

  - Options for the test environment vs. test application
  - Resolving conflicts when running multiple tests
  - Capturing standard output from the tests
  - Selecting which tests to run
  - Selecting which tests **not** to run

* **Integration tests**

  - Testing more than one unit at a time

* **Documentation tests**

  * Tests built into code documentation

--------------------
What Is a Unit Test?
--------------------

* Validates that an individual piece of code ("unit") behaves as designed

  * In Rust, typically a single subprogram

* Given a known set of inputs, output is predictable

  * If outputs do not match expectations then either ...

    * Code is wrong
    * Expectations are wrong

-------------------------
What Is in a Unit Test?
-------------------------

* The **FIRST** Principles

  * **Fast** - Run quickly for fast updates
  * **Isolated** - Do not rely on external interfaces
  * **Repeatable** - Deterministic outcomes
  * **Self-Validating** - Pass/fail without manual inspection
  * **Timely** - Written alongside or before implementation

* What to Test

  * **Happy Path** - Standard, expected usage scenarios.
  * **Edge Cases** - Zero, empty collections, null/None, boundaries
  * **Failure Modes** -  Expected panics, invalid inputs, and validation errors

.. note::

   Test **behavior and contracts**, not internal implementation details
