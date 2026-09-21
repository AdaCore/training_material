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

--------------------
What Is Unit Test?
--------------------

* Verification of "smallest testable unit" of an application

  * In Rust, typically a single subprogram

* Given a known set of inputs, output is predicatable

  * If outputs do not match expectations then either ...

    * Code is wrong
    * Expectations are wrong
