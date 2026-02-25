==========
Overview
==========

-----------------------
Rust Error Philosophy
-----------------------

* Errors are part of the type system

  * Rust treats failures as values

    * Not hidden control flow

* Core principles

  * Failures are explicit
  * Errors are visible in function signatures
  * The compiler helps ensure they are handled
  * Programs fail predictably, not mysteriously

.. list-table::
  :header-rows: 1

  * - Language Style
    - Error Visibility

  * - Exceptions
    - Hidden

  * - Error codes
    - Easy to ignore

  * - :rust:`Result`
    - Explicit and enforced

-----------------------------------------
Panic vs Result: Two Different Failures
-----------------------------------------

* Rust separates bugs from expected failures

* :rust:`panic!` |rightarrow| unrecoverable bug

  * Impossible state reached
  * Violated assumptions
  * Programmer error

* :rust:`Result` |rightarrow| expected failure

  * File not found
  * Network timeout
  * Invalid user input

.. code:: rust

  // Expected failure
  File::open("config.toml")?;

  // Bug: invariant violated
  assert!(index < len);
