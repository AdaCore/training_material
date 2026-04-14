==============
Try Operator
==============

------------------------
"?" - The Try Operator
------------------------

* Replaces repetitive match handling
* Keeps code focused on the "happy path"

* Automatically decodes :rust:`Result`

  * :rust:`Ok(value)` - **unwrap** value and continue execution
  * :rust:`Err(E)` - returns early
  * Works similarly for :rust:`Option`

* Limitation

  * Can only be used if function returns :rust:`Result` or :rust:`Option`
  
    * And return must be compatible with error being raised

-----------------------
Verbosity vs. Clarity
-----------------------

**Manual** :rust:`match`

  .. code:: rust

    fn get_data() -> Result<String, io::Error> {
        let res = File::open("config.txt");
        let mut file = match res {
            Ok(f) => f,
            Err(e) => return Err(e), // Explicit return
        };
        // ... repeat for every step ...
    }

**Try Operator** :rust:`?`

  .. code:: rust

    fn get_data() -> Result<String, io::Error> {
        let mut file = File::open("config.txt")?;
        let mut text = String::new();
        file.read_to_string(&mut text)?;
        Ok(text)
    }

----------------------------
Try Operator With "Option"
----------------------------

* :rust:`?` works with more than :rust:`Ok`/:rust:`Err`

  * Also works with :rust:`Some`/:rust:`None` from :rust:`Option`

* Behavior

  * If :rust:`Some(value)`, it evaluates to :rust:`value`
  * If :rust:`None`, the function returns :rust:`None` early

.. warning::

  **You cannot mix and match**

  Cannot use :rust:`?` on :rust:`Result` in function returning :rust:`Option`

    *Unless you convert explicitly*

--------------------------------
Returning "Result" From "main"
--------------------------------

* Rust allows :rust:`main` to return a :rust:`Result`

* If an error "bubbles up" to :rust:`main` and is returned then

  * Rust prints :rust:`Debug` representation of error
  * Exits with a non-zero error code

.. code:: rust

  fn main() -> Result<(), Box<dyn std::error::Error>> {
      let _file = File::open("essential_config.txt")?;
      Ok(())
  }

.. container:: latex_environment scriptsize

  :error:`Error: Os { code: 2, kind: NotFound, message: "No such file or directory" }`
