==================
Simple Unit Test
==================

-------------------------------
Creating a Library With Tests
-------------------------------

* :command:`cargo new` creates a new package with a *binary* target by default

  * Has its own :rust:`main` program

* Adding :command:`--lib` creates a package with a *library* target

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

.. note::

  :filename:`adder` is the folder created to contain the package

----------------------------------------
Creating Tests for an Existing Library
----------------------------------------

* Tests can be created outside of the file being tested
* Create :filename:`tests` folder at same level of unit being tested

  * To test :filename:`parser.rs`, create :filename:`tests` in same directory

* Modify :filename:`lib.rs` to reference test module

  .. code:: rust

    pub mod parser;

    #[cfg(test)]
    mod tests {
        mod parser_tests;
    }

* Create :filename:`parser_tests.rs` in :filename:`tests` folder
* :filename:`parser_tests.rs` contains a test submodule

  .. code:: rust

    use crate::parser;

    #[test]
    fn test_parser_internals() {
        // You have access to crate internals here
        assert!(parser::internal_helper());
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

* Line 5: Module only compiled when building tests
* Line 6: Creates a child module named :rust:`tests` 
* Line 7: Brings names from parent module into scope

  * Tests can call :rust:`add(...)` directly
  * :rust:`use super::*;` avoids qualified paths to parent-module items

* Line 9: Following function is a test

  * Test function takes no parameters
  * Usually returns :rust:`()`

    * Can also return :rust:`Result` (shown later)

* Line 11: Call to function being tested
* Line 12: Verify results of function call

----------------
Test Execution
----------------

:command:`rustc --test src/lib.rs`

  * Invokes compiler to build test executable

:command:`cargo test`

  * Build and run test executable

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
  * Executed behavior is consistent

* Single test *fails* when the test function panics

  * Panic fails current test
  * Remaining tests continue running

* Panic is most commonly caused by

  * Explicitly calling :rust:`panic!`
  * Assertion macros where assertion is false

    * :rust:`assert_eq!` (and :rust:`assert_ne!`)
    * :rust:`assert!`

----------------
Explicit Panic
----------------

**Use** :rust:`panic!` **to explicitly fail a test**

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

  * Two arguments supplied
  * Successful if arguments are equal (or not equal)

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

* Use :rust:`assert!` to verify a boolean expression

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

* When an assertion fails, current test stops executing

  * Remaining tests continue running

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
      assert!(add(1, 2) < result);
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
