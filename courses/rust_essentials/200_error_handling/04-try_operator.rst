==============
Try Operator
==============

------------------------
"?" - the Try Operator
------------------------

* Simplifying Error Propagation

  * Problem - manually matching every :rust:`Result` leads to deeply nested code
  * Solution - :rust:`?` operator provides a concise way to handle errors

* How it works

  * If value is :rust:`Ok(T)`, **unwrap** value and continue execution
  * If value is :rust:`Err(E)`, early return of error

* Limitation

  * Can only be used if function returns :rust:`Result` or :rust:`Option`
  
    * And return must be compatible with error being raised

----------------------
Verbosity vs Clarity
----------------------

* Manual match

  .. code:: rust

    fn get_data() -> Result<String, io::Error> {
        let res = File::open("config.txt");
        let mut file = match res {
            Ok(f) => f,
            Err(e) => return Err(e), // Explicit return
        };
        // ... repeat for every step ...
    }

* Try Operator

  .. code:: rust

    fn get_data() -> Result<String, io::Error> {
        let mut file = File::open("config.txt")?; 
        let mut s = String::new();
        file.read_to_string(&mut s)?; 
        Ok(s)
    }

---------------------------
Automatic Type Conversion
---------------------------

* :rust:`?` doesn't just return the error

  * It calls :rust:`Into::into` on it.

* Why this matters

  * Your function returns a custom :rust:`MyError` type **but**
  * Library function returns an :rust:`io::Error` **so**
  * :rust:`?` will automatically try to convert :rust:`io::Error` into :rust:`MyError`

    * Requirement: Your error type must implement :rust:`From<io::Error>`

* More information on this in the next chapter

----------------------------
Try Operator with "Option"
----------------------------

* :rust:`?` works with more than :rust:`Ok` / :rust:`Err`

  * It also works with :rust:`Some` /:rust:`None` from :rust:`Option`

* Behavior

  * If :rust:`Some(value)`, it evaluates to :rust:`value`
  * If :rust:`None`, the function returns :rust:`None` early

.. warning::

  **You cannot mix and match**

  You can't use :rust:`?` on a :rust:`Result` inside a function that returns an :rust:`Option`, and vice versa.

---------------
"?" in "main"
---------------

* Historically, :rust:`main` had to return :rust:`()`

  * So you couldn't use :rust:`?` there

* Modern Rust allows :rust:`main` to return a :rust:`Result`

* If an error "bubbles up" to :rust:`main` and is returned then

  * Rust prints :rust:`Debug` representation of error
  * Exits with a non-zero error code

.. code:: rust

  fn main() -> Result<(), Box<dyn std::error::Error>> {
      let _file = File::open("essential_config.txt")?;
      Ok(())
  }
