==================
Simple Unit Test
==================

-------------------
Unit Test in Rust
-------------------

* Rust uses an *attribute* to define a file's :rust:`tests` module

  .. code:: rust

    pub fn do_something() {}

    #[cfg(test)] // define test module for this file
    mod tests {
        use super::*;

        #[test]
        fn test1() {
            do_something();
        }
    }

* Running :command:`cargo test` on this crate says the test passes

  .. code:: output
    :font-size: tiny

    Compiling adder v0.1.0 (C:\temp\rust\testing\adder)
     Finished `test` profile [unoptimized + debuginfo] target(s) in 8.11s
      Running unittests src\lib.rs (target\debug\deps\adder-f5d94484c9544f76.exe)

    running 1 test
    test tests::test1 ... ok

    test result: ok. 1 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out; finished in 0.00s

       Doc-tests adder

    running 0 tests

    test result: ok. 0 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out; finished in 0.00s

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

---------------
Test Failures
---------------

* Failures cause a panic

  * Due to :rust:`assert_eq!` receiving non-matching values

* Adding a second (failing) test

  .. code:: rust

    #[test]
    fn doesnt_work() {
        let result = add(1, 3);
        assert_eq!(result, 5);
    }

* Generates a failing test execution

.. code:: output
  :font-size: tiny

  running 2 tests
  test tests::it_works ... ok
  test tests::doesnt_work ... FAILED

  failures:

  ---- tests::doesnt_work stdout ----

  thread 'tests::doesnt_work' (2084) panicked at src\lib.rs:18:9:
  assertion `left == right` failed
    left: 4
   right: 5
  note: run with `RUST_BACKTRACE=1` environment variable to display a backtrace
