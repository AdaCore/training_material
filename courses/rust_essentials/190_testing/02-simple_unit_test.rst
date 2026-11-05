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

:command:`cargo new adder --lib`

* Creates :filename:`adder` folder containing entire package

  * Including template source file :filename:`src/lib.rs`

  .. code:: rust
    :font-size: tiny

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

-----------------------------------
Creating Tests for a Binary Crate
-----------------------------------

* Tests can also be created in a binary crate

  * Require test engineer to add test code to file manually

* Unit tests in binary crate formatted same as in library crate

.. code:: rust

  fn helper(name: &str) -> String {
      format!("Hello, {}!", name)
  }

  fn main() {
      println!("Hello, world!");
      println!("{}", helper("Mickey"));
  }

  #[cfg(test)]
  mod tests {
      use super::*;

      #[test]
      fn test_helper() {
          assert_eq!(helper("foo"), "Hello, foo!")
      }
  }

------------------------------------------------
Library Crate Testing vs. Binary Crate Testing
------------------------------------------------

* Creating crate :rust:`my_crate` always creates

  * :filename:`my_crate` folder
  * :filename:`my_crate/src` folder
  * :filename:`Cargo.toml` file

* Library crates contain :filename:`my_crate/src/lib.rs`

  * Example function (:rust:`pub fn add`)
  * Example test configuration and module

* Binary crates contain :filename:`my_crate/src/main.rs`

  * Template main program
  * **No** test configuration module

    * It can be added to :filename:`main.rs`
    * But it can only test non-main functions

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
* Line 9: Following function is a test
* Line 10: Test function takes no parameters

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

* Individual test *fails* when function panics

  * If multiple tests defined, remaining tests continue running

* Panic is most commonly triggered by

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

* Tests often compare results to expected values

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

-------------------------------
Running Tests With Assertions
-------------------------------

:command:`cargo test`

.. code:: output
  :font-size: small

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
      assert_eq!(add(1, 1), 3); // Will fail
      assert_eq!(add(1, 1), 4); // Not reached
      assert_eq!(add(1, 1), 2); // Not reached
  }

  #[test]
  fn another_test() {
      let result = add(2, 2);
      assert!(add(1, 2) < result);
  }

----------------------------------------
Running Tests With Multiple Assertions
----------------------------------------

:command:`cargo test`

.. code:: output
  :font-size: footnotesize

  running 2 tests
  test tests::another_test ... ok
  test tests::multiple_assertions ... FAILED

  failures:

  ---- tests::multiple_assertions stdout ----

  thread 'tests::multiple_assertions' (54) panicked at src/lib.rs:11:9:
  assertion `left == right` failed
    left: 2
   right: 3
