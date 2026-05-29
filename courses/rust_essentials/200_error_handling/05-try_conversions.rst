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

  enum Reason {TooYoung, TooOld,}

  // Error type is 'Reason'
  fn check_age(age: i32) -> Result<i32, Reason> { }

  // Error type is 'String'
  fn register(age: i32) -> Result<i32, String> {
      // '?' sees 'Reason', knows the return type is 'String',
      // and converts it behind the scenes.
      check_age(age)?;
      Ok(age)
  }

  match register(10) {
      Ok(age) => println!("Good enough {age}"),
      Err(e) => eprintln!("Problem: {e}"),
  }
  match register(70) {
      Ok(()) => println!("Good enough {age}"),
      Err(e) => eprintln!("Problem: {e}"),
  }

:error:`Problem: The user is too young.`

:error:`Problem: The user is too old.`

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

  * Main logic remains clean and focused on "happy path"
  * :rust:`?` operator handles heavy lifting of error translation
