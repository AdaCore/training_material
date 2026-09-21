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
  - :rust:`Result` error variants can be checked

* **Running unit tests**

  - Many options to control how to run tests
  - Run tests in parallel or sequentially
  - Capturing output typically written to the console

    * Both standard output and standard error

  - Use pattern matching to find tests to run
  - Special attributes to prevent certain tests from running

    - Unless specifically requested
