========
Result
========

-------------------------
What Is "Result<T, E>"?
-------------------------

* Represents outcome of an operation that can succeed or fail

.. code:: rust

  enum Result<T, E> {
      Ok(T),   // success
      Err(E),  // failure
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

  :rust:`Result` is annotated with :rust:`#[must_uses]` attribute - it **cannot** be ignored

--------------
Common Usage
--------------

Besides match, Rust provides helpers

.. list-table::

  * - :rust:`.unwrap()`
    - Take the value or panic

  * - :rust:`.expect(msg)`
    - Panic with your message

  * - :rust:`.is_ok()` / :rust:`.is_err()`
    - Check the variant

--------------
"?" Operator
--------------

* :rust:`?` hides some of boilerplate of propagating errors up the call stack

  * I.e an early return on error

  * Before

    .. code:: rust

      let mut file = match File::create("my_best_friends.txt") {
             Err(e) => return Err(e),
             Ok(f) => f,
      };
      if let Err(e) = file.write_all(format!("name: {}\n", info.name).as_bytes()) {
          return Err(e)
      }
      if let Err(e) = file.write_all(format!("age: {}\n", info.age).as_bytes()) {
          return Err(e)
      }
      if let Err(e) = file.write_all(format!("rating: {}\n", info.rating).as_bytes()) {
          return Err(e)

  * After

    .. code:: rust

      let mut file = File::create("my_best_friends.txt")?;
      file.write_all(format!("name: {}\n", info.name).as_bytes())?;
      file.write_all(format!("age: {}\n", info.age).as_bytes())?;
      file.write_all(format!("rating: {}\n", info.rating).as_bytes())?;

* Ending expression with :rust:`?` results in the :rust:`Ok`’s unwrapped value

  * Unless result is Err, in which case Err is returned early
