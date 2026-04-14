=================
Try Conversions
=================

---------------------------------
Automatic Error Type Conversion
---------------------------------

:rust:`?` **doesn't just return the error**

* If error types match |rightarrow| returned directly
* If they differ |rightarrow| converted using :rust:`From`

.. code:: rust

  enum Reason { TooYoung, TooOld, }

  // Error type is 'Reason'
  fn check_age(age: i32) -> Result<i32, Reason> {
      Err(Reason::TooYoung)
  }

  // Error type is 'String'
  fn register() -> Result<(), String> {
      // '?' sees 'Reason', knows the return type is 'String',
      // and converts it behind the scenes.
      check_age(10)?; 
      Ok(())
  }

.. note::

  Return error type must implement :rust:`From` trait for source error type

  * Compiler verifies a valid path exists to convert the error
  * If not, it throws a *trait bound not satisfied* error

-------------------------------
One Return Type, Many Sources
-------------------------------

.. code:: rust

  fn initialize_system() -> Result<(), MyError> {
      // Convert 'io::Error' to 'MyError'
      let config = fs::read_to_string("config.json")?; 

      // Convert 'serde_json::Error' to 'MyError'
      let val: Config = serde_json::from_str(&config)?; 

      // Convert 'ParseIntError' to 'MyError'
      let port: u16 = val.port.parse()?; 

      Ok(())
  }

.. note::

  Main logic of the function remains clean and focused on the "happy path".

  The :rust:`?` operator handles the heavy lifting of error translation
