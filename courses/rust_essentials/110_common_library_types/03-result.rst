========
Result
========

-------------------
Why Use "Result"?
-------------------

* Provides explicit error handling

  * **Cannot** ignore errors silently
  * More error handling described later in the course

* Compiler warns if :rust:`Result` is unused

* Used pervasively for recoverable errors

  * I/O, parsing, etc.

.. Warning::

  :rust:`Result` is annotated with :rust:`#[must_use]` attribute - it **cannot** be ignored

-------------------------
What Is "Result<T, E>"?
-------------------------

* Mechanism to handle *recoverable* errors

.. code:: rust

  enum Result<T, E> {
      Ok(T),   // Success
      Err(E),  // Failure
  }

* :rust:`Result` carries information about the success or failure

  * :rust:`T` - type returned on success
  * :rust:`E` - type returned on failure

--------------
Common Usage
--------------

.. code:: rust

  fn divide(top: f64, bottom: f64) -> Result<f64, String> {
      if bottom == 0.0 {
          // Failure
          Err("Cannot divide by zero!".to_string())
      } else {
          // Success
          Ok(top / bottom)
      }
  }

  fn main() {
      let result = divide(10.0, 0.0);

      match result {
          Ok(value) => println!("Result: {value}"),
          Err(e)    => println!("Error: {e}"),
      }
  }
