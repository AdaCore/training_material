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
  - Dealing with expected errors

* **Running unit tests**

  - Options for the test environment vs. test application
  - Capturing standard output from the tests
  - Selecting which tests to run

* **Integration tests**

  - Testing more than one unit at a time

* **Documentation tests**

  * Tests built into code documentation

--------------------
What Is a Unit Test?
--------------------

* Validates that an individual piece of code ("unit") behaves as expected
* Typically tests a single function
* Given a known set of inputs, resulting behavior is predictable

  * If the test fails, then either...

    * Code is wrong
    * Test is wrong

------------------------------
Writing Effective Unit Tests
------------------------------

* The **FIRST** Principles

  * **Fast** - Run quickly for fast updates
  * **Isolated** - Do not rely on external interfaces
  * **Repeatable** - Deterministic outcomes
  * **Self-Validating** - Pass/fail without manual inspection
  * **Timely** - Written alongside or before implementation

* What to Test

  * **Happy Path** - Standard, expected usage scenarios
  * **Edge Cases** - Zero, empty collections, null/None, boundaries
  * **Failure Modes** -  Expected panics, invalid inputs, and validation errors

.. note::

   Test **behavior and contracts**, not internal implementation details
