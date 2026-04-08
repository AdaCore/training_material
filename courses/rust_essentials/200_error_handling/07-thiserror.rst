=============
"thiserror"
=============

---------------------------------
"Macro Magic" for Custom Errors
---------------------------------

* Implementing :rust:`Error` is tedious and error-prone

* :rust:`thiserror` crate provides a convenient **derive macro**

  * Generates all that code using simple attributes

.. note::

  :rust:`thiserror` comes from a crate - needs to be installed

-------------------
"error" Attribute
-------------------

.. code:: rust

  #[error("I/O error: {0}")]
  #[error("Source {path}")]

* :rust:`error` attribute generates :rust:`Display` implementation

  * Displays error message
  * String interpolation for dynamic values

    * Index to fill from tuple data
    * Name to fill from struct data

----------------------------------------
Defining Errors With "thiserror" Crate
----------------------------------------

.. code:: rust
  :number-lines: 1

  #[derive(Debug, Error)]
  enum MyError {
      #[error("I/O error: {0}")]
      IoError(#[from] io::Error),
      #[error("No text in {0}")]
      EmptyText(String),
  }

* Line 1: Generate :rust:`std::error::Error` implementation for :rust:`MyError`
* Line 3: Replace :rust:`fmt::Display` implementation

  * Uses this string with :rust:`IoError` value when printing error

* Line 5: Generate :rust:`impl From<io::Error>` for :rust:`MyError`

  * :rust:`?` will convert error into :rust:`MyError::IoError`

-----------------
More Attributes
-----------------

* :rust:`#[source]`

  * Implement :rust:`source` trait
  * Describes where the original error came from

* :rust:`#[from]`

  * Implement :rust:`From` trait for error variant
  * Automatically implements :rust:`#[source]`

* :rust:`#[error(transparent)]`

  * Uses :rust:`Display` original error

    * No need to write custom string

  * Automatically implements :rust:`source()` method

    * Returns underlying error

  * Does not add new message

----------------------
Why Use "thiserror"?
----------------------


* **Main reason:** to avoid manual implementation!

* Structured data

  * Enums allow :rust:`match` to handle specific errors

    * Rather than checking error message string

* Type safety

  * Compiler ensures that error messages match enum fields

* Ecosystem compatibility

  * Generates :rust:`std::error::Error` implementation
  * Errors work with other tools

    * Like standard library traits

-------------------------
"thiserror" in Practice
-------------------------

.. container:: latex_environment scriptsize

  .. code:: rust

    use thiserror::Error;
    use std::fs::File;

    #[derive(Error, Debug)]
    pub enum MyError {
        #[error("Environment variable {0} not set")]
        ConfigError(String),

        #[error("File system error")] // Automatically wraps io::Error
        IoError(#[from] std::io::Error),
    }

    fn main() -> Result<(), MyError> {
        // 1. Manual error creation
        let _ = Err(MyError::ConfigError("PORT".into()))?;

        // 2. Automatic conversion using ? (this returns IoError)
        let _f = File::open("missing.txt")?;

        Ok(())
    }

.. note::

  :rust:`_` and :rust:`_f` tell the compiler the objects are unused
