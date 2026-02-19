========
Result
========

-------------------------
What Is "Result<T, E>"?
-------------------------

* Represents outcome of an operation that can succeed or fail

.. code:: rust

  enum Result<T, E> {
      Ok(T),   // Success
      Err(E),  // Failure
  }


* :rust:`Result` carries information about *why* something failed

  * :rust:`T` is the type returned on success
  * :rust:`E` is the type returned on failure

----------------------
Why "Result" Matters
----------------------

* Encourages explicit error handling

  * Can’t ignore errors silently

* Compiler warns if :rust:`Result` is unused

* Used pervasively for recoverable errors

  * I/O, parsing, etc.

.. note::

  :rust:`Result` is annotated with :rust:`#[must_use]` attribute - it **cannot** be ignored

--------------
Common Usage
--------------

In addition to :rust:`match`, Rust provides helpers

.. list-table::

  * - :rust:`.unwrap()`
    - Take the value or panic

  * - :rust:`.expect(msg)`
    - Panic with your message

  * - :rust:`.is_ok()` / :rust:`.is_err()`
    - Check the variant
