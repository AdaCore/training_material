=================
Try Conversions
=================

----------------------------
Automatic Error Conversion
----------------------------

* **The Goal**

  * Functions often call multiple libraries, each returning a different error type

    * E.g., :rust:`io::Error`, :rust:`ParseIntError`, :rust:`Utf8Error`

* **The Problem**

  * Function can only return one error type in its :rust:`Result<T, E>`

* **The Solution**

  * :rust:`?` operator automatically converts *local* error into function's *return* error type

* **The Mechanism**

  * When you use :rust:`?`, Rust calls :rust:`.into()` on the error value behind the scenes

-----------------------
How it Works - "From"
-----------------------

* Only works if return error type implements :rust:`From` trait for source error type

* Standard Library Example

  * Your function returns :rust:`std::io::Error`
  * You call a function returning :rust:`std::net::AddrParseError`
  * Code won't compile unless there is an implementation like:

    .. code:: rust

      impl From<AddrParseError> for MyCustomError { ... }

* Compiler Check

  * Compiler verifies a valid path exists to convert the error
  * If not, it throws a *trait bound not satisfied* error

-------------------------------
One Return Type, Many Sources
-------------------------------

.. code:: rust

  fn initialize_system() -> Result<(), MySystemError> {
      // 1. Returns io::Error, converted to MySystemError
      let config = fs::read_to_string("config.json")?; 

      // 2. Returns serde_json::Error, converted to MySystemError
      let val: Config = serde_json::from_str(&config)?; 

      // 3. Returns ParseIntError, converted to MySystemError
      let port: u16 = val.port.parse()?; 

      Ok(())
  }

.. note::

  Main logic of the function remains clean and focused on the "happy path".

  The :rust:`?` operator handles the heavy lifting of error translation
