================
Test Execution
================

-------------------
Execution Options
-------------------

**Two categories of command-line options**

:command:`cargo test [test_options]`

  * Specify options for the test environment
  * :command:`cargo test --help` to see a list
  * Control compile vs. run, verbose mode, etc.

:command:`cargo test -- [binary_options]`

  * Specify options for the generated test executable
  * :command:`cargo test -- --help` to see a list
  * Control which test cases run, capturing output, etc.

-------------------
Some Example Code
-------------------

* A simple function under test

  * Divide by zero prints a message to standard error and returns 0
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

------------------------
Capturing Output (1/2)
------------------------

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

------------------------
Capturing Output (2/2)
------------------------

* To capture **all** output, use switch :command:`--show-output`

  * This shows stdandard output/error for each test

* :command:`cargo test -- --show-output`

.. code:: output

  running 3 tests
  test tests::normal ... ok
  test tests::error_output ... ok
  test tests::warning_output ... FAILED

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
        assert!(result > 0.0);
    }

* :rust:`really_long_test` will **not** be run via :command:`cargo test`

  * Run *only* ignored tests: :command:`cargo test -- --ignored`
  * Run *both* regular and ignored tests: :command:`cargo test -- --include-ignored`

-----------------------
More on Ignored Tests
-----------------------

* :rust:`ignore` attribute takes an optional string assignment for a message

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

* To find ignored tests, use :command:`cargo test -- --list --ignored`

  .. code:: output
    :font-size: scriptsize

    Running unittests src\lib.rs (target\debug\deps\adder-973b06.exe)
    tests::really_long_test: test
