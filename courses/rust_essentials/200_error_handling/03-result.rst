==========
"Result" 
==========

-------------------
Recoverable Error
-------------------

* Operational issues should be recoverable

  * File not found
  * Network timeout

* :rust:`Result` enum allows safe handling of problems

  .. container:: latex_environment footnotesize

    .. code:: rust

      enum Result<T, E> {
          Ok(T),  // Operation succeeded; contains value of type 'T'
          Err(E), // Operation failed; contains error of type 'E'
      }

.. note::

  You need to handle both good and bad variants!

------------------
Handling Results
------------------

* Pattern Matching (most explicit)

  .. code:: rust

    match File::open("data.txt") {
        Ok(file) => println!("File opened!"),
        Err(e)   => eprintln!("Failed to open: {e}"),
    }

* Convenience methods

  * :rust:`.unwrap()` - returns the value or panics
  * :rust:`.expect("Msg")` - like :rust:`unwrap`, with custom panic message
  * :rust:`.unwrap_or(default)` - fallback value on error

-----------------------
Results vs Exceptions
-----------------------

.. container:: latex_environment footnotesize

  .. list-table::
    :header-rows: 1

    * - **Feature**
      - :rust:`Result` **(Rust)**
      - :cpp:`try` / :cpp:`catch` **(other langauges)**

    * - *Visibility*
      - Part of function signature
      - Not part of function definition

    * - *Control Flow*
      - Explicitly handled
      - Bubbles up stack automatically

    * - *Performance*
      - Low cost (enum variants)
      - Higher cost (runtime overhead)

    * - *Safety*
      - Compiler forces you to handle error
      - Easy to forget :cpp:`catch` block

--------------------
Propagating Errors
--------------------

* Instead of handling the error ...

  * ... you can return it to the caller (manually)

* Shortcut - :rust:`?` operator makes propagation concise

  * Called the **Try Operator** - see next chapter for details

.. container:: latex_environment footnotesize

  .. code:: rust

    fn read_username() -> Result<String, io::Error> {
        // "open" returns Result
        //   "?" returns to caller if Result is Err
        let mut file = File::open("user.txt")?;
        let mut text = String::new();
        // "read_to_string" returns Result
        //   "?" returns to caller if Result is Err
        file.read_to_string(&mut text)?;
        Ok(text)
    }

.. note::

  Use :rust:`?` when current function wants caller to deal with error
