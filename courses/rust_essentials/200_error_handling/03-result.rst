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

* Helper methods

  * :rust:`.unwrap()` - returns the value or panics
  * :rust:`.expect("Msg")` - like :rust:`unwrap`, with custom panic message
  * :rust:`.unwrap_or(default)` - fallback value on error

-----------------------
Results vs Exceptions
-----------------------

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
