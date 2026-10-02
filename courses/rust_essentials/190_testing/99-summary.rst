=========
Summary
=========

-----------------
What We Covered
-----------------

* **Building unit tests**

  - Integrated part of the Rust ecosystem
  - Built into the source code being tested

    - But not compiled into deliverable code

  - Track result failures using assertions or panics

* **Improving unit tests**

  - Assertion failures can include descriptions
  - Expected panics can be caught and verified
  - Tests can return :rust:`Result` and propagate errors with :rust:`?`

* **Running unit tests**

  - Many options to control how to run tests
  - Run tests in parallel or sequentially
  - Capturing output typically written to the console

    * Both standard output and standard error

  - Filter tests by a substring of their full name
  - Special attributes to prevent certain tests from running

    - Unless specifically requested
