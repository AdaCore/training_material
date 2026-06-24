==========
"Result"
==========

-------------------
Recoverable Error
-------------------

* Operational issues should be recoverable

  * File not found
  * Network timeout

* :rust:`Result` allows safe handling of any outcome

  .. container:: latex_environment footnotesize

    .. code:: rust

      enum Result<T, E> {
          Ok(T),  // Success - contains value of type 'T'
          Err(E), // Failure - contains error of type 'E'
      }

.. note::

  Both variants need to be handled

------------------
Handling Results
------------------

* Pattern Matching (idiomatic)

  .. code:: rust

    match File::open("data.txt") {
        Ok(file) => println!("File opened!"),
        Err(e)   => eprintln!("Failed to open: {e}"),
    }

:error:`Failed to open: No such file or directory (os error 2)`

----------------
Helper Methods
----------------

.. code:: rust

  let good: Result<i32, &str> = Ok(42);
  let bad: Result<i32, &str> = Err("Problem");

* :rust:`.unwrap()` - returns the value or panics

  .. code:: rust

    println!("Good: {}", good.unwrap());
    println!("Bad: {}", bad.unwrap());

  .. code:: output

    Good: 42
    thread 'main' panicked at src\main.rs:5:27:
    called `Result::unwrap()` on an `Err` value: "Problem"

* :rust:`.expect("Msg")` - like :rust:`unwrap`, with custom panic message

  .. code:: rust

    println!("Good: {}", good.expect("Expected"));
    println!("Bad: {}", bad.expect("Expected"));

  .. code:: output

    Good: 42
    thread 'main' panicked at src\main.rs:5:27:
    Expected: "Problem"

* :rust:`.unwrap_or(default)` - fallback value on error

  .. code:: rust

    println!("Good: {}", good.unwrap_or(-1));
    println!("Bad: {}", bad.unwrap_or(-1));

  .. code:: output

    Good: 42
    Bad: -1

------------------------
Results vs. Exceptions
------------------------

.. container:: latex_environment footnotesize

  .. list-table::
    :header-rows: 1
    :stub-columns: 1

    * - **Feature**
      - :rust:`Result` **(Rust)**
      - :cpp:`try` **/** :cpp:`catch` **(other languages)**

    * - *Visibility*
      - Part of function signature
      - Not part of function definition

    * - *Control Flow*
      - Explicitly handled
      - Bubbles up stack automatically

    * - *Performance*
      - Low cost
      - Runtime overhead

    * - *Safety*
      - Error must be handled
      - Easy to forget :cpp:`catch` block

--------------------
Propagating Errors
--------------------

* Instead of handling error - return it to caller (manually)

* **Shortcut:** :rust:`?` operator makes propagation concise

  * Called the :dfn:`Try Operator`

.. container:: latex_environment footnotesize

  .. code:: rust

    fn open_file(filename: &str) -> Result<File, io::Error> {
        // 'open' returns 'Result'
        // '?' returns to caller if 'Result' is 'Err'
        let mut file = File::open("user.txt")?;
        Ok(file)
    }

.. note::

  Use :rust:`?` when current function wants caller to deal with error
