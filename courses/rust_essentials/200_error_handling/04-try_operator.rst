==============
Try Operator
==============

--------------------------
What Is the Try Operator
--------------------------

* :dfn:`Try operator (?)` - syntax used to propagate errors by either

  * Unwrapping :rust:`Result` if it is the success variant
  * Immediately returning the failure variant

* Replaces repetitive match handling
* Keeps code focused on the "happy path"

* Automatically decodes :rust:`Result`

  * :rust:`Ok(value)` - **unwrap** value and continue execution
  * :rust:`Err(E)` - returns early

* Can only be used if function returns :rust:`Result`
  
  * Return must be compatible with error being raised

-----------------------
Clarity vs. Verbosity
-----------------------

**Try Operator** :rust:`?`

  .. code:: rust

    fn get_data() -> Result<String, io::Error> {
        let mut file = File::open("config.txt")?;
        let mut text = String::new();
        file.read_to_string(&mut text)?;
        Ok(text)
    }

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

----------------------------
Try Operator With "Option"
----------------------------

* :rust:`?` also works with :rust:`Some` / :rust:`None` from :rust:`Option`

* Behavior

  * :rust:`Some(value)` - evaluates to :rust:`value`
  * :rust:`None` - function returns :rust:`None` early

.. warning::

  **You cannot mix and match**

  Cannot use :rust:`?` on :rust:`Result` in function returning :rust:`Option`

--------------------------------
Returning "Result" From "main"
--------------------------------

* :rust:`main` can return :rust:`Result`

* If an error "bubbles up" to :rust:`main` and is returned

  * Prints :rust:`Debug` representation of error
  * Exits with a non-zero error code

.. code:: rust

  use std::fs::File;

  fn main() -> std::io::Result<()> {
      let _file = File::open("essential_config.txt")?;
      Ok(())
  }

.. container:: latex_environment scriptsize

  :error:`Error: Os { code: 2, kind: NotFound, message: "No such file or directory" }`
