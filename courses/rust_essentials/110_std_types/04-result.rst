========
Result
========

--------------------------
What Is "Result<<T, E>"?
--------------------------

* :rust:`Result<T, E>` represents the outcome of an operation that can succeed or fail

.. code:: rust

  enum Result<T, E> {
      Ok(T),   // success
      Err(E),  // failure
  }


* :rust:`Result` carries information about *why* something failed

  * rust:`T` is the type returned on success
  * rust:`E` is the type returned on failure

----------------------
Why "Result" Matters
----------------------

* Encourages explicit error handling

  * Can’t ignore errors silently

* Compiler warns if :rust:`Result` is unused

* Used pervasively for recoverable errors

  * I/O, parsing, etc.

--------------
Common Usage
--------------

Besides match, Rust provides helpers:

  * :rust:`.unwrap()` – take the value or panic

  * :rust:`.expect(msg)` – panic with your message

  * :rust:`.is_ok()` / :rust:`.is_err()` – check the variant
