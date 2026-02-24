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

-------------------------------------
Make Invalid States Unrepresentable
-------------------------------------

* Rust pushes correctness into the type system

  * Instead of accepting "any number"

    .. code:: rust

      fn find_user(id: u64) -> Result<User, Error>

  * We can define a stronger type:

    .. code:: rust

      struct UserId(u64);

      fn find_user(id: UserId) -> Result<User, Error>

* Now

  * You cannot accidentally pass an unrelated :rust:`u64`
  * You must consciously construct a :rust:`UserId`
  * The API documents intent through types
