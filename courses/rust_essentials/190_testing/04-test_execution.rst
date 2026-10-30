================
Test Execution
================

-----------------------------------
Controlling Execution Environment
-----------------------------------

:command:`cargo test [test_options]`

* Some examples

  * :command:`--no-run` |rightarrow| Compile without executing
  * :command:`--package` |rightarrow| Package selection
  * :command:`--all-features` |rightarrow| Activate all available features

* Plus many others

--------------------------------
Controlling Execution Behavior
--------------------------------

:command:`cargo test -- [binary_options]`

* Some examples

  * :command:`--ignored` |rightarrow| Run only tests with :rust:`#[ignore]` attribute
  * :command:`--skip <FILTER>` |rightarrow| Skip tests based on a filter
  * :command:`--quiet` |rightarrow| Use short form for status

* Plus many others

-------------------
Some Example Code
-------------------

* A simple function under test

  * Divide by zero prints a message to standard error and returns 0.0
  * Otherwise return the result of the division

    * Print a warning to standard output when dividing by a small number

.. code:: Rust

  pub fn divide(top: f64, bottom: f64) -> f64 {
      if bottom == 0.0 {
          eprintln!("Problem - divide by 0");
          0.0_f64
      } else if bottom.abs() < 1.0 {
          println!("Warning - this might get large");
          top / bottom
      } else {
          top / bottom
      }
  }

* Some empty (for now) tests

  .. code:: Rust

    #[test]
    fn error_output() {}

    #[test]
    fn warning_output() {}

    #[test]
    fn normal() {}

    #[test]
    fn really_long_test() {}

------------------------
Running Multiple Tests
------------------------

* Default behavior is to run all tests in parallel

  * Result order is non-deterministic

  .. code:: output
    :font-size: tiny

    running 4 tests
    test tests::error_output ... ok
    test tests::normal ... ok
    test tests::really_long_test ... ok
    test tests::warning_output ... ok

* If tests require system resources, parallel execution can cause conflicts

  * Need to enforce execution of one test at a time
  * :command:`cargo test -- --test-threads=1`

  .. code:: output

    running 4 tests
    test tests::error_output ... ok
    test tests::normal ... ok
    test tests::really_long_test ... ok
    test tests::warning_output ... ok

.. note::

  Specify value higher than 1 to run tests in parallel
  but reduce system load

-----------------------------
Dealing With Console Output
-----------------------------

* Unit tests for three paths

  * Normal path - no output
  * Warning path - output to standard output
  * Error path - output to standard error

.. code:: Rust

  #[cfg(test)]
  mod tests {
      use super::*;
      #[test]
      fn error_output() {
          let result = divide(14.0, 0.0);
          assert_eq!(result, 0.0);
      }
      #[test]
      fn warning_output() {
          let result = divide(14.0, 0.1);
          assert!(result == 0.0);
      }
      #[test]
      fn normal() {
          let result = divide(14.0, 1.0);
          assert!(result > 0.0);
      }
  }

---------------------------------------
Default Behavior for Capturing Output
---------------------------------------

* By default, output only shows for failing tests

  .. code:: output
    :font-size: small

    running 3 tests
    test tests::normal ... ok
    test tests::error_output ... ok
    test tests::warning_output ... FAILED

    failures:

    ---- tests::warning_output stdout ----
    Warning - this might get large

    thread 'tests::warning_output' panicked at src\lib.rs:26:9:
    assertion failed: result == 0.0

* :rust:`normal` and :rust:`error_output` passed

  * No output displayed

* :rust:`warning_output` failed

  * Console output is part of test results

----------------------
Capturing All Output
----------------------

* To show output from successful tests, use switch :command:`--show-output`

  * This shows standard output/error for each test

* :command:`cargo test -- --show-output`

.. code:: output

  ...
  successes:

  ---- tests::error_output stdout ----
  Problem - divide by 0

  successes:
      tests::error_output
      tests::normal

  failures:

  ---- tests::warning_output stdout ----
  Warning - this might get large

  thread 'tests::warning_output' panicked at src\lib.rs:26:9:
  assertion failed: result == 0.0

  failures:
      tests::warning_output

------------------------
Selecting Tests to Run
------------------------

* :command:`cargo test` will run all tests in the source

* Can select a specific test by specifying the test name

  * :command:`cargo test error_output`

  .. code:: output

    running 1 test
    test tests::error_output ... ok

* Can also specify part of a name

  * Runs any test that includes the string in the name

  * :command:`cargo test output`

  .. code:: output
    :font-size: small

    running 2 tests
    test tests::error_output ... ok
    test tests::warning_output ... FAILED

    failures:

    ---- tests::warning_output stdout ----
    Warning - this might get large

    thread 'tests::warning_output' panicked at src\lib.rs:26:9:
    assertion failed: result == 0.0

-------------------------------------------
Preventing Problematic Tests From Running
-------------------------------------------

* Some tests should not be run as part of normal testing

  * Tests that take a long time
  * Tests that depend on unavailable resources

* Can flag these tests with :rust:`#[ignore]` attribute

  .. code:: Rust

    #[test]
    #[ignore]
    fn really_long_test() {
        let result = divide(14.0, 0.0);
        std::thread::sleep(std::time::Duration::from_millis(500));
        assert!(result == 0.0);
    }

* :rust:`really_long_test` will **not** be run via :command:`cargo test`
* :command:`cargo test -- --ignored` |rightarrow| Run *only* ignored tests
* :command:`cargo test -- --include-ignored` |rightarrow| Run *both* regular and ignored tests

-----------------------
More on Ignored Tests
-----------------------

* :rust:`#[ignore]` attribute takes an optional string assignment for a message

  .. code:: Rust

    #[test]
    #[ignore = "This takes a while"]
    fn really_long_test() {
        let result = divide(14.0, 0.0);
        std::thread::sleep(std::time::Duration::from_millis(500));
        assert!(result == 0.0);
    }

  * When running :command:`cargo test`, ignored tests show the message

    .. code:: output
      :font-size: footnotesize

      running 4 tests
      test tests::really_long_test ... ignored, This takes a while
      test tests::error_output ... ok
      test tests::normal ... ok
      test tests::warning_output ... FAILED

* :command:`cargo test -- --list --ignored` |rightarrow| Find ignored tests

  .. code:: output
    :font-size: scriptsize

    Running unittests src\lib.rs (target\debug\deps\adder-973b06.exe)
    tests::really_long_test: test
