===================
Integration Tests
===================

------------------------------
What is an Integration Test?
------------------------------

* An :dfn:`integration test` is a test application that

  * Combines units, modules, and/or subsystems
  * Verifies

    * Interfaces and contracts for subprogram calls
    * Data persistence
    * Interactions between outside sources
    * Shared resource contention

* In Rust, integration tests are

  * Located in their own :filename:`tests` folder
  * Allowed to test public APIs only
  * Used to generate end-to-end workflows

--------------------
The "tests" Folder
--------------------

* :filename:`tests` folder created at same level as :filename:`src` folder

  * Test files stored in that folder (names are not specific)

* Test file content similar to :rust:`tests` module in library unit

  * No :rust:`mod tests` required
  * No implicit import, so :rust:`use` clause necessary

-------------------------------
Example Source and Test Files
-------------------------------

:filename:`src/lib.rs`

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

:filename:`tests/integration_tests.rs`

.. code:: rust

  use adder::add;

  #[test]
  fn add_positive() {
      let result = add(2, 3);
      assert_eq!(result, 5);
  }

---------------------------
Running Integration Tests
---------------------------

* Integration tests are run as part of :command:`cargo test`

.. code:: output
  :font-size: tiny

  running 1 test
  test tests::it_works ... ok

  test result: ok. 1 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out; finished in 0.00s

       Running tests\integration_test.rs (target\debug\deps\integration_test-932a510f81a09fa9.exe)

  running 1 test
  test add_positive ... ok

  test result: ok. 1 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out; finished in 0.00s

     Doc-tests adder

  running 0 tests

  test result: ok. 0 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out; finished in 0.00s

.. note:: 

  *Doctests* covered later

---------------------------------
Submodules in Integration Tests
---------------------------------

* What about common functions for multiple tests?

  * Things like setting up global data or releaseing a resource

* Test helpers need to be hidden from testing mechanism

  * :command:`cargo test` tests all Rust files in :filename:`tests` folder

* Solution is to create a subfolder in :filename:`tests` 

  * Using old style naming conventions
  * e.g. :filename:`common/mod.rs`

    * Creates module :rust:`common`
    * Tests can import :rust:`common` and call its functions

-----------------------
Testing Binary Crates
-----------------------

* :command:`cargo test` cannot test :filename:`main.rs`

  * It's already an application

* Solution is to have main functionality in a library crate

  * "Main" crate is now testable
  * :rust:`main` imports this "main" crate

    * :rust:`main` now is just a simple call

.. tip::

  This concept of splitting main application from functionality
  works well for other languages too!
