=============
"thiserror"
=============

---------------------------------
"Macro Magic" for Custom Errors
---------------------------------

* **The Problem**

  * Manually implementing :rust:`Display`, :rust:`Debug`, and :rust:`Error::source` for every enum is tedious and error-prone

* **The Solution**

  * :rust:`thiserror` crate provides a convenient **derive macro**

    * Generates all that code for you using simple attributes

* **Primary Use Case**

  * Best suited for libraries
  * Provide specific, structured error types that callers can match against

* **Design Goal**

  * Zero-cost and minimal runtime overhead
  * Writes the code you would have written yourself

--------------
Basic Syntax
--------------

* Define error as enum and use :rust:`#[error(...)]` attribute to define :rust:`Display` message

.. code:: rust

  use thiserror::Error;

  #[derive(Error, Debug)]
  pub enum DataStoreError {
      #[error("data store disconnected")]
      Disconnect(#[from] io::Error), // Automatically implements From<io::Error>!

      #[error("the data for key `{0}` is not found")]
      NotFound(String),

      #[error("invalid header (expected {expected:?}, found {found:?})")]
      InvalidHeader {
          expected: String,
          found: String,
      },

      #[error("unknown data store error")]
      Unknown,
  }

---------------------------
Attributes that Save Time
---------------------------

* :rust:`#[error("...")]`

  * Defines the :rust:`Display` implementation
  * Reference enum fields using :rust:`{}` or :rust:`{0}` for tuple variants

* :rust:`#[from]`

  * Automatically implements :rust:`From<SourceError>` for enum
  * Makes the :rust:`?` operator work instantly for that error type

* :rust:`#[source]`

  * Explicitly marks a field as the underlying cause of the error

    * Used by :rust:`Error::source()`
    * Note: :rust:`#[from]` implies :rust:`#[source]`

* **Transparent Errors**

  * Use :rust:`#[error(transparent)]` to forward :rust:`Display` and :rust:`source methods` an underlying error
  * Does not add a new message

----------------------
Why Use "thiserror"?
----------------------

* **Structured Data**

  * Enums allow your users to use match to handle specific errors

    * E.g., "retry if disconnected, but stop if not found"
    * Unlike using a simple string

* **Type Safety**

  * Compiler ensures that error messages match enum fields

* **Ecosystem Compatibility**

  * Generates a standard :rust:`std::error::Error` implementation
  * So errors work perfectly with other tools like :rust:`anyhow` or standard library traits

---------------------
"thiserror" in Action
---------------------

.. container:: latex_environment scriptsize

  .. code:: rust

    fn get_config(path: &str) -> Result<Config, DataStoreError> {
        // If File::open fails, it returns io::Error. 
        // #[from] attribute converts it to DataStoreError::Disconnect automatically
        let content = fs::read_to_string(path)?; 

        if content.is_empty() {
            return Err(DataStoreError::NotFound(path.to_string()));
        }

        Ok(parse_config(content))
    }
