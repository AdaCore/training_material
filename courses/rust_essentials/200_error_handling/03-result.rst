==========
"Result" 
==========

----------------------------------
Recoverable Errors with "Result"
----------------------------------

* Core error handling mechanism

  * Problem: Many operations can fail for reasons beyond the programmer's control

    * E.g., file not found, network timeout

  * Solution: Use the :rust:`Result<T, E>` enum

    * Instead of throwing an exception or returning a "magic" error code

* Enum definition

  .. code:: rust

    enum Result<T, E> {
        Ok(T),  // The operation succeeded; contains value of type T
        Err(E), // The operation failed; contains error of type E
    }

.. note::

  Enforces *Type Safety* - cannot access success value without first checking if result is :rust:`Ok`

------------------
Handling Results
------------------

* Pattern Matching

  * Most explicit way of handling errors

  .. code:: rust

    match File::open("data.txt") {
        Ok(file) => println!("File opened!"),
        Err(e)   => eprintln!("Failed to open: {e}"),
    }

* Convenience methods

  * :rust:`.unwrap()` - returns the value or panics if an error
  * :rust:`.expect("Msg")` - like unwrap, but with a custom panic message
  * :rust:`.unwrap_or(default)` - provides a fallback value if an error occurs

-----------------------
Results vs Exceptions
-----------------------

.. container:: latex_environment footnotesize

  .. list-table::
    :header-rows: 1

    * - Feature
      - :rust:`Result`
      - :cpp:`try` / :cpp:`catch`

    * - **Visibility**
      - Part of function signature
      - Often hidden

    * -
      - (You know it can fail)
      - (You might not know it throws)

    * - **Control Flow**
      - Explicitly handled
      - Bubbles up stack automatically

    * -
      - (via matching or propagation)
      -

    * - **Performance**
      - Low cost
      - Higher cost

    * -
      - (Just an enum return)
      - (Requires stack unwinding)

    * - **Safety**
      - Compiler forces you to handle error
      - Easy to forget :cpp:`catch` block

--------------------
Propagating Errors
--------------------

* Manual propagation - return the error up to the caller (verbose!)

* Shortcut - :rust:`?` operator makes propagation concise

  * Called the **Try Operator** - see next chapter for details

.. container:: latex_environment footnotesize

  .. code:: rust

    fn read_username() -> Result<String, io::Error> {
        let mut f = File::open("user.txt")?; // Returns early on Err
        let mut s = String::new();
        f.read_to_string(&mut s)?;           // Returns early on Err
        Ok(s)
    }

.. note::

  General rule - use :rust:`?` when current function wants the 
  caller to deal with the error
