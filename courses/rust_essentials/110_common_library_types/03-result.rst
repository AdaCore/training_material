========
Result
========

-------------------
Why Use "Result"?
-------------------

* Encourages explicit error handling

  * Can’t ignore errors silently
  * More error handling described later in the course

* Compiler warns if :rust:`Result` is unused

* Used pervasively for recoverable errors

  * I/O, parsing, etc.

.. Warning::

  :rust:`Result` is annotated with :rust:`#[must_use]` attribute - it **cannot** be ignored

-------------------------
What Is "Result<T, E>"?
-------------------------

* Mechanism to handle recoverable errors

.. code:: rust

  enum Result<T, E> {
      Ok(T),   // Success
      Err(E),  // Failure
  }

* :rust:`Result` carries information about *why* something failed

  * :rust:`T` is the type returned on success
  * :rust:`E` is the type returned on failure

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
