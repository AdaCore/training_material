==================
Simple Unit Test
==================

-------------------------------
Creating a Testable Framework
-------------------------------

* :command:`cargo new` by default creates an application crate

  * Which has its own :rust:`main` program

* Adding :command:`--lib` creates a "library" crate template

  * No main program
  * :rust:`tests` module automatically inserted

    * Contains an example test

:command:`cargo new adder --lib`

.. code:: rust
  :font-size: footnotesize
  :number-lines: 1

  pub fn add(left: u64, right: u64) -> u64 {
      left + right
  }

  #[cfg(test)]
  mod tests {
      use super::*;

      #[test]
      fn it_works() {
          let result = add(2, 2);
          assert_eq!(result, 4);
      }
  }

----------------
Test Semantics
----------------

.. code:: rust
  :font-size: footnotesize
  :number-lines: 5

  #[cfg(test)]
  mod tests {
      use super::*;

      #[test]
      fn it_works() {
          let result = add(2, 2);
          assert_eq!(result, 4);
      }
  }

* Line 5: Module only compiled when running :command:`cargo test`
* Line 6: Test module (:rust:`tests` is idiomatic but other names allowed)
* Line 7: Add items from enclosing module into scope
* Line 9: Following function is a test

  * Typically no parameters or return values

* Line 10: Call to function being tested
* Line 11: Verify results of function call

----------------
Test Execution
----------------

:command:`cargo test`

.. code:: output
  :font-size: tiny

     Compiling adder v0.1.0 (C:\temp\rust\testing\adder)
      Finished `test` profile [unoptimized + debuginfo] target(s) in 4.78s
       Running unittests src\lib.rs (target\debug\deps\adder-f5d94484c9544f76.exe)

  running 1 test
  test tests::it_works ... ok

  test result: ok. 1 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out; finished in 0.01s

     Doc-tests adder

  running 0 tests

  test result: ok. 0 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out; finished in 0.00s

.. note::

  :dfn:`Doc-tests` (documentation tests) will be discussed later

-------------------------
Validating Test Results
-------------------------

* Generally, a test is validated by comparing results to expectations

  * Output matches an expected value
  * Output is in a range of values
  * Output scenario is consistent

* Single test *fails* when the test function panics

  * Panic stops at test boundary - multiple tests can fail

* Panic is most commonly caused by

  * Explicitly calling :rust:`panic!`
  * Assertion macros where assertion is false

    * :rust:`asserteq!` (and :rust:`assert_ne!`)
    * :rust:`assert!`

----------------
Explicit Panic
----------------

**It is easy to test results and cause a panic**

.. code:: rust
  :font-size: footnotesize

  #[test]
  fn explicit_panic() {
      let result1 = add(1, 1);
      let result2 = add(2, 2);
      if result1 != 1 || result2 != 4 {
          panic!("Something went wrong");
      }
  }

.. code:: output
  :font-size: tiny

  running 1 test
  test tests::explicit_panic ... FAILED

  failures:

  ---- tests::explicit_panic stdout ----

  thread 'tests::explicit_panic' (33352) panicked at src\lib.rs:14:13:
  Something went wrong

---------------------------------
"assert_eq!" (and "assert_ne!")
---------------------------------

* Most common form of validation is equality

  * :rust:`2 + 2 == 4`
  * :rust:`sqrt(144) == 12`

* :rust:`assert_eq!` and :rust:`assert_ne!` simplify the equality check

  * Two parameters passed in
  * Successful if parameters are equal (or not equal)

.. code:: rust
  :font-size: footnotesize

  #[test]
  fn passing_test() {
      let result = add(2, 2);
      assert_eq!(result, 4);
  }

  #[test]
  fn failing_test() {
      let result = add(2, 2);
      assert_ne!(result, 4);
  }

.. code:: output
  :font-size: tiny

  running 2 tests
  test tests::passing_test ... ok
  test tests::failing_test ... FAILED

  failures:

  ---- tests::failing_test stdout ----

  thread 'tests::failing_test' (185) panicked at src/lib.rs:18:9:
  assertion `left != right` failed
    left: 4
   right: 4

-----------
"assert!"
-----------

* More complicated comparisons use :rust:`assert!`

  * One parameter - a boolean expression

.. code:: rust
  :font-size: footnotesize

  #[test]
  fn assert_test() {
      assert!(add(1,2) == add(3,4));
  }

.. code:: output
  :font-size: tiny

  running 1 test
  test tests::assert_test ... FAILED

  failures:

  ---- tests::assert_test stdout ----

  thread 'tests::assert_test' (205) panicked at src/lib.rs:24:9:
  assertion failed: add(1, 2) == add(3, 4)

---------------------
Multiple Assertions
---------------------

* Tests with multiple assertions will panic on the first assertion failure

  * But will continue to run any other tests

.. code:: rust
  :font-size: footnotesize

  #[test]
  fn multiple_assertions() {
      assert_eq!(add(1, 1), 3); // Should fail
      assert_eq!(add(1, 1), 4); // Should fail
      assert_eq!(add(1, 1), 2); // Should pass
  }

  #[test]
  fn another_test() {
      let result = add(2, 2);
      assert!(add(1, 2) < add(3, 4));
  }

.. code:: output
  :font-size: tiny

  running 2 tests
  test tests::another_test ... ok
  test tests::multiple_assertions ... FAILED

  failures:

  ---- tests::multiple_assertions stdout ----

  thread 'tests::multiple_assertions' (54) panicked at src/lib.rs:11:9:
  assertion `left == right` failed
    left: 2
   right: 3
